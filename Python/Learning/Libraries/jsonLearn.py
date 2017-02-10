import json

def writeData():
    objp = {"value1": 20, "value2": 30, "valueTest": "hehe"}
    with open('data.json', 'w') as outfile:
        json.dump(json.JSONEncoder().encode(objp), outfile)

def readData():
    with open('data.json') as json_data:
        d = json.JSONDecoder().decode(json.load(json_data))
        return d

# writeData()
programData = readData()
print(programData.get("valueTest"))
