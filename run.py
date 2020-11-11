import sys
from PyQt5.QtWidgets import QApplication, QWidget, QPushButton
from PyQt5.QtGui import QIcon
from PyQt5.QtCore import pyqtSlot
import random
import sys

button_size = 20
width = 30
height = 20
p =  0.1

colorDict = {0:"black", 1:"blue", 2:"green", 3:"red", 4: "darkblue", 5:"brown", 6:"cyan", 7:"black", 8:"darkred"}


class ButtonWrapper:

    def __init__(self, widget, x, y, isBomb, observer):
        self.x = x
        self.y = y
        self.isBomb = random.random() < p
        self.button = QPushButton(widget)
        self.button.setFixedHeight(button_size)
        self.button.setFixedWidth(20)
        self.button.move(y*button_size,x*button_size)
        self.button.clicked.connect(self.callback)
        self.observer = observer

    def callback(self):
        self.button.setEnabled(False)
        self.button.setCheckable(True)
        self.button.toggle()
        self.observer.notify(self)

class Manager:

    def __init__(self, widget):
        self.widget = widget
        self.buttons = [[ButtonWrapper(widget, i, j, False, self) for i in range(height+1)] for j in range(width+1)]
        self.knowledgeList = []
        self.bombs = []
        self.suggestingLockingButton = None
        self.suggestedButton = None

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
            self.knowledgeList.append((x, y, cnt))
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

        while True:
            b = self.buttons[random.randrange(0, width)][random.randrange(0, height)]
            if not b.isBomb and b.button.isEnabled():
                b.button.setStyleSheet('QPushButton {background-color: red;}')
                self.suggestedButton = b
                break
                



def window():
   app = QApplication(sys.argv)
   widget = QWidget()

   manager = Manager(widget)

   widget.resize(button_size*width, button_size*height)
   widget.setWindowTitle("PyQt5 Button Click Example")
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
