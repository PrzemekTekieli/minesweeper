import sys
from PyQt5.QtWidgets import QApplication, QWidget, QPushButton
from PyQt5.QtGui import QIcon
from PyQt5.QtCore import pyqtSlot, QRunnable, QThreadPool
from pyswip import Prolog
import time
import json
import random
import sys

prolog = Prolog()
prolog.consult("main.pl")
list(prolog.query("start(5,5)"))
button_size = 20
width = 30
height = 20
p =  0.1
mode = 0

colorDict = {0:"black", 1:"blue", 2:"green", 3:"red", 4: "darkblue", 5:"brown", 6:"cyan", 7:"black", 8:"darkred"}


class ButtonWrapper:

    def __init__(self, widget, x, y, isBomb, observer):
        self.x = x
        self.y = y
        self.isBomb = random.random() < p
        self.button = QPushButton(widget)
        self.button.setFixedHeight(button_size)
        self.button.setFixedWidth(button_size)
        self.button.move(y*button_size,x*button_size)
        self.button.clicked.connect(self.callback)
        self.observer = observer

    def callback(self):
        self.button.setEnabled(False)
        self.button.setCheckable(True)
        self.button.toggle()
        self.observer.notify(self)

class Worker(QRunnable):

    def __init__(self, *args, **kwargs):
        super(Worker, self).__init__()
        self.args = args
        self.kwargs = kwargs
        self.suggestedButton = self.args[0]

    @pyqtSlot()
    def run(self):
        time.sleep(1)
        global mode
        if(mode == 1):
            self.suggestedButton.button.click()  

class Manager:

    def __init__(self, widget):
        self.widget = widget
        self.buttons = [[ButtonWrapper(widget, i, j, False, self) for i in range(height+1)] for j in range(width+1)]
        self.knowledgeList = []
        self.totalKnowledgeList = []
        self.bombs = []
        self.suggestingLockingButton = None
        self.suggestedButton = None
        self.threadpool = QThreadPool()

        for l in self.buttons:
            for b in l:
                if b.isBomb:
                    self.bombs.append(b)

    def notify(self, buttonWrapper):
        if(buttonWrapper.isBomb) :
            print("Game Over")
            for bomb in self.bombs:
                bomb.button.setStyleSheet('QPushButton {background-color: red; font-weight: bold; color: black;}')
                bomb.button.setText("*")
            for l in self.buttons:
                for b in l:
                    b.button.setEnabled(False)
            # self.widget.close()

        else:
            x = buttonWrapper.x
            y = buttonWrapper.y
            cnt = 0
            for i in range(max(0,x-1), min(height,x+1)+1):
                for j in range(max(0,y-1), min(width,y+1)+1):
                    if (i != x or j != y) and self.buttons[j][i].isBomb:
                        cnt = cnt + 1

            buttonWrapper.button.setStyleSheet('QPushButton {background-color: #A3C1DA; font-weight: bold; color: ' + colorDict[cnt] + ';}')
            self.knowledgeList.append(([x, y], cnt))
            list(prolog.query(f'assert_fields([[{x+1},{y+1}],{cnt}])'))
            # print("Knowledge list: ", self.knowledgeList)
            if self.suggestingLockingButton is None:
                self.suggestingLockingButton = buttonWrapper
            
            if cnt != 0:
                buttonWrapper.button.setText(str(cnt))  
            else:
                self.clickNeighbourhood(buttonWrapper)
            
            if self.suggestingLockingButton == buttonWrapper:
                self.suggestingLockingButton = None
                self.suggestNextStep()

            

    def clickNeighbourhood(self, buttonWrapper):
        x = buttonWrapper.x
        y = buttonWrapper.y
        for i in range(max(0,x-1), min(height,x+1)+1):
            for j in range(max(0,y-1), min(width,y+1)+1):
                if(self.buttons[j][i].button.isEnabled()):
                    self.buttons[j][i].button.click()


    def suggestNextStep(self):

        if self.suggestedButton is not None and self.suggestedButton.button.isEnabled():
            self.suggestedButton.button.setStyleSheet('QPushButton {background-color: white;}')

        marked_bombs = []

        resultList = json.loads(str(list(prolog.query("next_step(Fields,Mines)"))).replace("'", '"'))
        if(len(resultList)==0):
            return
        result = resultList[0]
        print(result)
        marked_bombs += result['Mines']

        while (len(result['Fields']) == 0):
            resultList = json.loads(str(list(prolog.query("next_step(Fields,Mines)"))).replace("'", '"'))
            if(len(resultList)==0):
                return
            result = resultList[0]
            print(result)
            marked_bombs += result['Mines']

        for coords in marked_bombs:
            bomb = self.buttons[coords[1]-1][coords[0]-1]
            bomb.button.setStyleSheet('QPushButton {background-color: gray; font-weight: bold; color: red;}')
            bomb.button.setText("X")

        field = result['Fields'] if type(result['Fields'][0]) is not list else result['Fields'][0]
        print(field)
        b = self.buttons[field[1]-1][field[0]-1]
        b.button.setStyleSheet('QPushButton {background-color: red;}')
        self.suggestedButton = b

        if mode == 1:
            worker = Worker(self.suggestedButton)
            self.threadpool.start(worker)



    def suggestNextStepOld(self):

        if self.suggestedButton is not None and self.suggestedButton.button.isEnabled():
            self.suggestedButton.button.setStyleSheet('QPushButton {background-color: white;}')

        while True:
            b = self.buttons[random.randrange(0, width)][random.randrange(0, height)]
            if not b.isBomb and b.button.isEnabled():
                b.button.setStyleSheet('QPushButton {background-color: red;}')
                self.suggestedButton = b
                break


class PlayModeButtonWrapper:

    def __init__(self, widget, manager, h, w):
        self.button = QPushButton(widget)
        self.button.setFixedHeight(2*button_size)
        self.button.setFixedWidth(3*button_size)
        self.button.move(w,h)
        self.button.setText('MODE')
        self.button.clicked.connect(self.callback)
        self.manager = manager

    def callback(self):
        global mode
        if mode == 1:
            mode = 0
        else:
            mode = 1
            if(self.manager.suggestedButton is not None):
                self.manager.suggestedButton.button.click()
        print(mode)
          
def window():
   app = QApplication(sys.argv)
   widget = QWidget()
   list(prolog.query(f"start({height},{width})"))
   
   manager = Manager(widget)
   modeButton = PlayModeButtonWrapper(widget, manager, (height + 1) * button_size, ((width - 1) * button_size) / 2)

   widget.resize(button_size*width, button_size*(height+3))
   widget.setWindowTitle("Minesweeper")
   widget.show()   
   sys.exit(app.exec_()) 
   
if __name__ == '__main__':
    argsLen = len(sys.argv)
    if(argsLen > 1):
        width = int(sys.argv[1])
    if(argsLen > 2):
        height = int(sys.argv[2])
    if(argsLen > 3):
        p = float(sys.argv[3])
    window()
