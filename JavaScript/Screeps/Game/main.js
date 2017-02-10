var roleHarvester = require('role.harvester');
var roleUpgrader = require('role.upgrader');
var roleBuilder = require('role.builder');

module.exports.loop = function () {

    for(var name in Memory.creeps) {
        if(!Game.creeps[name]) {
            delete Memory.creeps[name];
            console.log('Clearing after: ' + name);
        }
    }

    var harvesters = _.filter(Game.creeps, (creep) => creep.memory.role == 'harvester');
    var upgraders = _.filter(Game.creeps, (creep) => creep.memory.role == 'upgrader');
    var builders = _.filter(Game.creeps, (creep) => creep.memory.role == 'builder');

    if(harvesters.length < 3) {
        var newNameH = Game.spawns.MainNexus.createCreep([WORK,WORK,CARRY,MOVE,MOVE], undefined, {role: 'harvester'});
        console.log('Spawning new harvester: ' + newNameH);
    } else if(upgraders.length < 3) {
        var newNameU = Game.spawns.MainNexus.createCreep([WORK,WORK,CARRY,MOVE,MOVE], undefined, {role: 'upgrader', working: true});
        console.log('Spawning new upgrader: ' + newNameU);
    } else if(builders.length < 3) {
        var newNameB = Game.spawns.MainNexus.createCreep([WORK,WORK,CARRY,MOVE,MOVE], undefined, {role: 'builder'});
        console.log('Spawning new builder: ' + newNameB);
    }

    for(var name in Game.creeps) {
        var creep = Game.creeps[name];
        if(creep.memory.role == 'harvester') {
            roleHarvester.run(creep);
        }
        if(creep.memory.role == 'upgrader') {
            roleUpgrader.run(creep);
        }
        if(creep.memory.role == 'builder') {
            roleBuilder.run(creep);
        }
    }
}
