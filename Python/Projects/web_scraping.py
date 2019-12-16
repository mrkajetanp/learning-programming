
from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from bs4 import BeautifulSoup
import pyperclip

chrome_options = Options()
chrome_options.add_argument("--headless")

driver = webdriver.Chrome(options = chrome_options)
driver.get("http://unblockpirate.uk/user/surferbroadband/")

content = driver.page_source
soup = BeautifulSoup(content, features="html.parser")

links = soup.findAll('a', href=True)

desc = str([s for s in links if "Wall Street Journal" in str(s)][0])
magnet = str([s for s in links if "Wall+Street+Journal" in str(s)][0])

desc = desc[desc.find("\">W")+2:-4]
magnet = magnet[9:magnet.find("title=")-2]

print(desc)
print()
print(magnet)

pyperclip.copy(magnet)

