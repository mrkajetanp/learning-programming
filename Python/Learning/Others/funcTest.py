

def generatePositions(start, nitems):
    if start%2==0: return [x for x in range(start, start+nitems*2) if x%2==0]
    if start%2!=0: return [x for x in range(start, start+nitems*2) if x%2!=0]

class SimpleMenu:
    def __init__(self, cases, results):
        self.menuCases = cases
        self.caseResults = results
    def printMenu(self):
        print(self.menuCases)
        print(self.caseResults)

cases = ["White", "Green", "Red", "Blue", "Yellow", "Magenta"]

menuOne = SimpleMenu(cases, ["A", "B"])

menuOne.printMenu()
