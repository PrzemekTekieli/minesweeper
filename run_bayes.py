from PyQt5.QtWidgets import QApplication, QWidget, QPushButton
from PyQt5.QtGui import QIcon
from PyQt5.QtCore import pyqtSlot, QRunnable, QThreadPool
import time
import json
import random
import itertools
import bayes
import sys

button_size = 40
width = 3
height = 3
p =  0.1
mode = 0
bombs_amount = 2
total_knowledge_mode = False

colorDict = {0:"black", 1:"blue", 2:"green", 3:"red", 4: "darkblue", 5:"brown", 6:"cyan", 7:"black", 8:"darkred"}

class ButtonWrapper:

    def __init__(self, widget, x, y, isBomb, observer):
        self.x = x
        self.y = y
        self.isBomb = False
        self.button = QPushButton(widget)
        self.button.setFixedHeight(button_size)
        self.button.setFixedWidth(button_size)
        self.button.move(y*button_size,x*button_size)
        self.button.clicked.connect(self.callback)
        self.observer = observer
        self.clicked = False

    def placeBomb(self):
        self.isBomb = True

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
        time.sleep(0.3)
        global mode
        if(mode == 1):
            self.suggestedButton.button.click()  

class Manager:

    def __init__(self, widget, ms):
        self.widget = widget
        self.buttons = [[ButtonWrapper(widget, i, j, False, self) for i in range(height)] for j in range(width)]
        self.knowledgeList = []
        self.totalKnowledgeList = []
        self.suggestingLockingButton = None
        self.suggestedButton = None
        self.allSuggestedButtons = []
        self.threadpool = QThreadPool()
        self.initial = True
        self.ms = ms
        self.ms.update_knowledge()

        self.bombs = []


    def initBombs(self, x, y):
        notClicked = []
        for b in list(itertools.chain.from_iterable(self.buttons)):
            if not (b.x == x and b.y == y):
                notClicked.append(b)

        self.bombs = random.sample(notClicked, bombs_amount)
        for button in self.bombs:
            button.placeBomb()


    
    def notify(self, buttonWrapper):

        if self.initial:
            self.initial = False
            self.initBombs(buttonWrapper.x, buttonWrapper.y)

        if(buttonWrapper.isBomb) :
            print("Game Over")
            for bomb in self.bombs:
                bomb.button.setStyleSheet('QPushButton {background-color: red; font-weight: bold; color: black;}')
                bomb.button.setText("*")
            for l in self.buttons:
                for b in l:
                    b.button.setEnabled(False)

        else:
            x = buttonWrapper.x
            y = buttonWrapper.y
            cnt = 0
            buttonWrapper.clicked = True
            for i in range(max(0,x-1), min(height-1,x+1)+1):
                for j in range(max(0,y-1), min(width-1,y+1)+1):
                    if (i != x or j != y) and self.buttons[j][i].isBomb:
                        cnt = cnt + 1

            buttonWrapper.button.setStyleSheet('QPushButton {background-color: #A3C1DA; font-weight: bold; color: ' + colorDict[cnt] + ';}')
            self.knowledgeList.append(([x, y], cnt))

            if cnt != 0:
                buttonWrapper.button.setText(str(cnt))  
            else:
                buttonWrapper.button.setText('')
                self.clickNeighbourhood(buttonWrapper)
            buttonWrapper.clicked = True

            self.ms.set_field(y+1,x+1,cnt)
            self.ms.update_knowledge()

            for i in range(height):
                for j in range(width):
                    field = self.buttons[j][i]
                    if not field.clicked and not (i == x and j == y):
                        val = self.ms.get_field(j+1,i+1)
                        print(val)
                        field.button.setText(str(round(val[1], ndigits=3)))

    def getNextAvailableSuggestedButton(self):
        if self.suggestedButton is None:
            if len(self.allSuggestedButtons) > 0:
                field = self.allSuggestedButtons.pop(0)
                b = self.buttons[field[1]-1][field[0]-1]
                self.suggestedButton = b
        
        while (self.suggestedButton.button.isEnabled() == False) and (len(self.allSuggestedButtons) > 0):
            field = self.allSuggestedButtons.pop(0)
            b = self.buttons[field[1]-1][field[0]-1]
            self.suggestedButton = b

    def clickNeighbourhood(self, buttonWrapper):
        x = buttonWrapper.x
        y = buttonWrapper.y
        for i in range(max(0,x-1), min(height-1,x+1)+1):
            for j in range(max(0,y-1), min(width-1,y+1)+1):
                if(self.buttons[j][i].button.isEnabled()):
                    self.buttons[j][i].button.click()


    def suggestNextStep(self):
        self.ms.update_knowledge()

        
class PlayModeButtonWrapper:

    def __init__(self, widget, manager, h, w):
        self.button = QPushButton(widget)
        self.button.setFixedHeight(2*button_size)
        self.button.setFixedWidth(3*button_size)
        self.button.move(w,h)
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


class PlayModeKnowledgeButtonWrapper:

    def __init__(self, widget, manager, h, w):
        self.button = QPushButton(widget)
        self.button.setFixedHeight(2*button_size)
        self.button.setFixedWidth(10*button_size)
        self.button.move(w,h)
        self.button.setText('SHOW ALL SUGGESTED')
        self.button.clicked.connect(self.callback)
        self.manager = manager

    def callback(self):
        global total_knowledge_mode
        if total_knowledge_mode:
            total_knowledge_mode = False
        else:
            total_knowledge_mode = True
          
def window():

   ms = bayes.MineSweeper(width,height)
   ms.set_mine_count(bombs_amount)
   ms.update_knowledge()

   app = QApplication([])
   widget = QWidget()

   manager = Manager(widget, ms)

   widget.resize(button_size*width, button_size*height)
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
        bombs_amount = int(sys.argv[3])
    window()