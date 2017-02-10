// spawning creep with a memory

Game.spawns.Spawn1.createCreep( [WORK, CARRY, MOVE], 'Builder1', { role: 'builder' } );

// role - Builder

var roleBuilder = {

    /** @param {Creep} creep **/
    run: function(creep) {

	      if(creep.memory.building && creep.carry.energy == 0) {
            creep.memory.building = false;
	      }
	      if(!creep.memory.building && creep.carry.energy == creep.carryCapacity) {
	          creep.memory.building = true;
	      }

	      if(creep.memory.building) {
	          var targets = creep.room.find(FIND_CONSTRUCTION_SITES);
            if(targets.length) {
                if(creep.build(targets[0]) == ERR_NOT_IN_RANGE) {
                    creep.moveTo(targets[0]);
                }
            }
	      }
	      else {
	          var sources = creep.room.find(FIND_SOURCES);
            if(creep.harvest(sources[0]) == ERR_NOT_IN_RANGE) {
                creep.moveTo(sources[0]);
            }
	      }
	  }
};

module.exports = roleBuilder;

// redefining harvesters to maintain extensions as well

var roleHarvester = {

    /** @param {Creep} creep **/
    run: function(creep) {
	      if(creep.carry.energy < creep.carryCapacity) {
            var sources = creep.room.find(FIND_SOURCES);
            if(creep.harvest(sources[0]) == ERR_NOT_IN_RANGE) {
                creep.moveTo(sources[0]);
            }
        }
        else {
            var targets = creep.room.find(FIND_STRUCTURES, {
                filter: (structure) => {
                    return (structure.structureType == STRUCTURE_EXTENSION || structure.structureType == STRUCTURE_SPAWN) &&
                        structure.energy < structure.energyCapacity;
                }
            });
            if(targets.length > 0) {
                if(creep.transfer(targets[0], RESOURCE_ENERGY) == ERR_NOT_IN_RANGE) {
                    creep.moveTo(targets[0]);
                }
            }
        }
	  }
};

module.exports = roleHarvester;

// logging total amount of energy in the room to the console

for(var name in Game.rooms) {
    console.log('Room "'+name+'" has '+Game.rooms[name].energyAvailable+' energy');
}

// spawning a bigger creep who works faster

Game.spawns.Spawn1.createCreep( [WORK,WORK,WORK,WORK,CARRY,MOVE,MOVE],
                                'HarvesterBig',
                                { role: 'harvester' } );


