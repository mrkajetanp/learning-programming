// Game - Main Global Game Object

// ***** Game object's properties ***** \\

Game.constructionSites -> object<string, ConstructionSite>
// A hash containing all your construction sites with their id as hash keys.

Game.cpu -> object
// Obj with information about your CPU usage with properties.
    limit -> number // CPU limit
    tickLimit -> number // availabile CPU time at the current game tick
    bucket  -> number // amount of unused CPU accumulated in your bucket

Game.creeps -> object<string, Creep>
    // hash containing all your creeps with creep names as hash keys
    for(var i in Game.creeps) {
        Game.creeps[i].moveTo(flag);
    }

Game.flags -> object<string, Flag>
    // A hash containing all your flags with their names as keys
    creep.moveTo(Game.flags.Flag1);

Game.gcl -> object
    // Your Global Control Level with properties
    level -> number // the current level
    progress -> number // the current progress to the next level
    progressTotal -> number // progress required to the next level

Game.map -> Map
    // Global object representing world map

Game.market -> Market
    // Global object representing the in-game market

Game.rooms -> object<string, Room>
    // hash containing all rooms available to you with names as keys

Game.spawns -> object<string, StructureSpawn>
    // a hash with all your spawns with spawn names as hash keys
    for(var i in Game.spawns) {
        Game.spawns[i].createCreep(body);
    }

Game.structures -> object<string, Structure>
    // hash with all your structures with structure id as hash keys

Game.time -> number
    // System game tick counter. Returns current tick.


// ***** Game object's methods ***** \\


Game.cpu.getUsed() -> used CPU as float number
    // get amount of cpu used from the beginning of the current game tick
    if(Game.cpu.getUsed() > Game.cpu.tickLimit / 2) {
        console.log("Used half of CPU already!");
    }
    for(var name in Game.creeps) {
        var startCpu = Game.cpu.getUsed();
        // creep logic in here
        var elapsed = Game.cpu.getUsed() - startCpu;
        console.log('Creep' + name + ' has used ' + elapsed + ' CPU time.');
    }

Game.getObjectById(id) -> object instance or null if not found
    // Get an object with the specified unique ID. (From visible rooms)
    id <- string // the unique ID
    creep.memory.sourceId = creep.pos.findClosestByRange(FIND_SOURCES).id;
    var source = Game.getObjectById(creep.memory.sourceId);

Game.notify(message, [groupInterval])
    // send a custom message to your profile email (notifications to yourself)
    message <- string // custom text which will be sent in the message (1000ch)
    groupInterval <- number // when will it be sent, 0 - immediately
    if(creep.hits < creep.memory.lastHits) {
        Game.notify(creep + ' has been attacked at ' + creep.pos + '!');
    }
    if(Game.spawns.Spawn1.energy == 0) {
        Game.notify('Spawn1 is out of energy', 180); // 180 minutes - 3 hours
    }


// Map Object (Game.map)
// Methods

describeExits(roomName) // {*LOW*}
    // List all exits available from the room with the given name
    Arguments: roomName <- string - the room name
    Returns:
    {
        "1": "W8N4",    // TOP
        "3": "W7N3",    // RIGHT
	      "5": "W8N2",    // BOTTOM
	      "7": "W9N3"     // LEFT
    }
    var exits = Game.map.describeExits('W8N3');

findExit(fromRoom, toRoom, [opts]) // {*HIGH*}
    // Find the exit direction from the given room en route to another.
    Arguments:
    fromRoom <- string(room name) or Room(room object)
    toRoom <- string, Room
    opts (optional) <- object // with pathfinding options
    Return:
    // The room direction constant, one of the following:

    FIND_EXIT_TOP
    FIND_EXIT_RIGHT
    FIND_EXIT_BOTTOM
    FIND_EXIT_LEFT

    Or one of the following error codes:
    ERR_NO_PATH 	-2 	Path can not be found.
    ERR_INVALID_ARGS 	-10 	The location is incorrect.

findRoute(fromRoom, toRoom, [opts]) // {*HIGH*}
    // find route from room to another room

getRoomLinearDistance(roomName1, roomName2) // {*NONE*}
    // gets distance (in rooms) between two rooms
    // You can use this function to estimate the energy cost of sending resources through terminals, or using observers and nukes.

getTerrainAt(x, y, roomName) // {*LOW}
getTerrainAt(pos) // {*LOW*}
    // Get terrain type at the specified room position.

isRoomProtected(roomName) // {*AVERAGE*}
    // Check if the room is protected by temporary "newbie" walls.

// PathFinder
// Static Methods

PathFinder.search(origin, goal, [opts]) // {*HIGH*}
    // Find an optimal path between origin and goal.

PathFinder.use(isEnabled) // {*NONE*}
    // Specify whether to use new experimental pathfinding.

// PathFinder.CostMatrix

// Container for custom navigation cost data. By default PathFinder will only consider terrain data (plain, swamp, wall) — if you need to route around obstacles such as buildings or creeps you must put them into a CostMatrix. Generally you will create your CostMatrix from within roomCallback. If a non-0 value is found in a room's CostMatrix then that value will be used instead of the default terrain cost. You should avoid using large values in your CostMatrix and terrain cost flags. For example, running PathFinder.search with { plainCost: 1, swampCost: 5 } is faster than running it with {plainCost: 2, swampCost: 10 } even though your paths will be the same. 

// Constructor

PathFinder.CostMatrix()
    // Creates a new CostMatrix containing 0's for all positions

// Methods

set(x,y,cost)
    // Set the cost of a position in this CostMatrix

get(x, y)
    // Gets the cost

clone()
    // clones

serialize()
    // representation in JSON

Static Method:
    PathFinder.CostMatrix.deserialize(val)  // {*LOW*}
    // Static method which deserializes a new CostMatrix using the return value of serialize.








