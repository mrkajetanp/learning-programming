// printing the number of creeps to the console

var harvesters = _.filter(Game.creeps, (creep) => creep.memory.role == 'harvester');
console.log('Harvesters: ' + harvesters.length);

// spawning new creep if one dies (number of creeps decreases)

if(harvesters.length < 2) {
    var newName = Game.spawns.Spawn1.createCreep([WORK,CARRY,MOVE], undefined, {role: 'harvester'});
    console.log('Spawning new harvester: ' + newName);
}

// commanding a creep to commit suicide

Game.creeps.Harvester1.suicide()

// Clearing memory of dead creeps
// Always place this memory cleaning code at the very top of your main loop!

for(var name in Memory.creeps) {
    if(!Game.creeps[name]) {
        delete Memory.creeps[name];
        console.log('Clearing non-existing creep memory:', name);
    }
}




