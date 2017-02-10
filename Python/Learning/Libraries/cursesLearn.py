import curses

# IDEA: Think about positioning menus differently depending on a size of the terminal

def generatePositions(start, nitems):
    if start%2==0: return [x for x in range(start, start+nitems*2) if x%2==0]
    if start%2!=0: return [x for x in range(start, start+nitems*2) if x%2!=0]

def setupConfigs():
    curses.curs_set(False)
    curses.init_pair(1, curses.COLOR_BLACK, curses.COLOR_WHITE)
    curses.init_pair(2, curses.COLOR_GREEN, curses.COLOR_BLACK)
    curses.init_pair(3, curses.COLOR_RED, curses.COLOR_BLACK)
    curses.init_pair(4, curses.COLOR_BLUE, curses.COLOR_BLACK)
    curses.init_pair(5, curses.COLOR_YELLOW, curses.COLOR_BLACK)
    curses.init_pair(6, curses.COLOR_MAGENTA, curses.COLOR_BLACK)

class SimpleMenu:
    menuX = 5
    resultMessageY = 20
    currPos = 0
    def __init__(self, stdscr, cases, results, posData):
        self.menuCases = cases
        self.caseResults = results
        self.positions = generatePositions(posData[0], posData[1])
        self.stdscr = stdscr
    def drawMenu(self):
        for i in range(0,6):
            self.stdscr.addstr(self.positions[i], self.menuX, self.menuCases[i], curses.A_BOLD)
        self.stdscr.refresh()
    def pickCase(self, stdscr, currPos):
        if(currPos == 0):
            self.stdscr.addstr(self.resultMessageY, self.menuX, self.caseResults[0], curses.color_pair(0))
        elif(currPos == 1):
            self.stdscr.addstr(self.resultMessageY, self.menuX, self.caseResults[1], curses.color_pair(2))
        elif(currPos == 2):
            self.stdscr.addstr(self.resultMessageY, self.menuX, self.caseResults[2], curses.color_pair(3))
        elif(currPos == 3):
            self.stdscr.addstr(self.resultMessageY, self.menuX, self.caseResults[3], curses.color_pair(4))
        elif(currPos == 4):
            self.stdscr.addstr(self.resultMessageY, self.menuX, self.caseResults[4], curses.color_pair(5))
        elif(currPos == 5):
            self.stdscr.addstr(self.resultMessageY, self.menuX, self.caseResults[5], curses.color_pair(6))
    def keyActions(self, key):
        self.stdscr.addstr(self.resultMessageY, self.menuX, " " * int(len(self.caseResults[0])+20))
        if(key == "KEY_DOWN" and self.currPos != len(self.positions)-1):
            self.stdscr.addstr(self.positions[self.currPos], self.menuX, self.menuCases[self.currPos], curses.A_BOLD)
            self.stdscr.addstr(self.positions[self.currPos+1], self.menuX, self.menuCases[self.currPos+1], curses.color_pair(1))
            self.currPos += 1
            self.stdscr.refresh()
        elif(key == "KEY_UP" and self.currPos != 0):
            self.stdscr.addstr(self.positions[self.currPos], self.menuX, self.menuCases[self.currPos], curses.A_BOLD)
            self.stdscr.addstr(self.positions[self.currPos-1], self.menuX, self.menuCases[self.currPos-1], curses.color_pair(1))
            self.currPos -= 1
            self.stdscr.refresh()
        elif(key == '\n' or key == "KEY_ENTER"):
            self.pickCase(self.stdscr, self.currPos)
        elif(key == "q"): exit()
    def loop(self):
        self.stdscr.addstr(self.positions[self.currPos], self.menuX, self.menuCases[self.currPos], curses.color_pair(1))
        self.stdscr.refresh()
        while True:
            key = self.stdscr.getkey()
            self.keyActions(key)
    def printMenu(self):
        print(self.menuCases)
        print(self.caseResults)

def main(stdscr):
    setupConfigs()
    menuCases = ["White", "Green", "Red", "Blue", "Yellow", "Magenta"]
    caseResults = ["White text!", "Green text!", "Red text!",
                  "Blue text!", "Yellow text!", "Magenta text!"]

    menuOne = SimpleMenu(stdscr, menuCases, caseResults, [4, 6])
    menuTwo = SimpleMenu(stdscr, menuCases, caseResults, [4, 6])
    menuOne.menuX = 10
    menuTwo.menuX = 45

    menuOne.drawMenu()
    menuTwo.drawMenu()

    menuOne.loop()
    # TODO: Connect the menus so you can loop between menuOne and menuTwo

curses.wrapper(main)
