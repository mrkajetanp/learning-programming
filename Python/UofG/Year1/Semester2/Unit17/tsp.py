import math
import Canvas

def distance(x1, y1, x2, y2):
    return math.sqrt(((x2 - x1)**2 + (y2 - y1)**2))

def get_data(name):
    with open(name) as data:
        lines = [l.split(" ") for l in data.readlines()]
        return { l[2].strip() : [int(l[0]), int(l[1])] for l in lines}

def find_nearest_to(data, tour, city):
    cities = list(data.keys())
    cities.remove(city)

    city = data[city]
    nearest = 1
    nearest_distance = distance(city[0], city[1], data[cities[1]][0], data[cities[1]][1])
    while cities[nearest] in tour:
        nearest += 1
        nearest_distance = distance(city[0], city[1],
                                    data[cities[nearest]][0], data[cities[nearest]][1])
    for i in range(2, len(cities)):
        new_distance = distance(city[0], city[1], data[cities[i]][0], data[cities[i]][1])
        if cities[i] not in tour and new_distance < nearest_distance:
            nearest = i
            nearest_distance = new_distance

    return cities[nearest], nearest_distance


data = get_data("Cities.txt")
cities = list(data.keys())
tour = [cities[0]]

Canvas.create_text(data[tour[-1]][0], data[tour[-1]][1], text=tour[-1])
full_distance = 0
for i in range(1, len(cities)):
    nearest_city, local_distance = find_nearest_to(data, tour, tour[-1])
    full_distance += local_distance
    Canvas.create_line(data[tour[-1]][0], data[tour[-1]][1],
                       data[nearest_city][0], data[nearest_city][1])
    tour.append(nearest_city)
    Canvas.create_text(data[tour[-1]][0],data[tour[-1]][1], text=tour[-1])

Canvas.create_text(220, 240, text=("Full distance: {}".format(round(full_distance))))

Canvas.run()

