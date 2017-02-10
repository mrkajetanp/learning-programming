from cursesmenu import *
from cursesmenu.items import *

menu = CursesMenu("Title", "Subtitle")

menu_item = MenuItem("Menu Item")
function_item = FunctionItem("Call a Python function", input, ["Enter an input"])
command_item = CommandItem("Run command", "ls")
selection_menu = SelectionMenu(['Item 1', 'Item 2', 'Item 3'])
submenu_item = SubmenuItem("Submenu item", selection_menu, menu)

menu.append_item(menu_item)
menu.append_item(function_item)
menu.append_item(command_item)
menu.append_item(submenu_item)

menu.show()
