#!/usr/bin/env ruby

def size ; 500 ; end
def timeLimit ; 90 * 1000 ; end
def numCraters ; 20 ; end
def numBoulders ; 20 ; end 
def numEnemies ; 100 ; end 

params1 = <<EOPARAM
{
        "maxSpeed" : 20,
        "accel" : 2,
        "brake" : 3,
        "turn" : 20,
        "hardTurn" : 60,
    	"rotAccel" : 120,
        "frontView" : 60,
        "rearView" : 30
}
EOPARAM


def craters ; (1..numCraters).map{randomObject } ; end
def boulders ; (1..numBoulders).map{randomObject } ; end

def randpos ;  size * (rand - 0.5) ; end
def randdir ;  360 * rand ; end
def randspeed ;  2 * rand ; end # ???
def randview ;  60 * rand ; end # ???
def randradius ;  10 * rand ; end

def vehicle ; [randpos, randpos, randdir] ; end
def enemies ; (1..numEnemies).map{ enemy } ; end
def enemy ; [randpos, randpos, randdir, randspeed, randview] ; end
def randomObject ; [randpos, randpos, randradius] ; end

def pointToJson(p)
  return (" { \"x\" : %.3f, \"y\" : %.3f, \"r\" : %.3f }\n " % p)
end

def objectsToJson(list) 
  "[" + 
    list.map { |point|
      pointToJson(point);
    }.join(", ") +
  "]\n"
end

def listToJson(list, converter)  
  "[ #{list.map{|elem| converter.call(elem)}.join(", ")}  ]\n"
end

def vehicleToJson( v )
   " { \"x\" : %.3f, \"y\" : %.3f, \"dir\" : %.3f }\n" % v 
end

def enemyToJson( e )
  (" { \"x\" : %.3f, \"y\" : %.3f, \"dir\" : %.3f, \"speed\" : %.3f, \"view\" : %.3f }\n" % e )
end

def runsToJson(runs) 
  listToJson(runs, proc {|x| runToJson x })
end

def runToJson(run)
  "{ \"vehicle\" : " + 
    vehicleToJson(run[0]) +
    ", \"enemies\" : " +
    listToJson(run[1], proc {|x| enemyToJson x }) +
  "}\n"
end

def run
  [vehicle, enemies]
end

runs =  (1..5).map{ |i| run }

marsMap = <<EOMAP 
{
"size" : #{size},
"timeLimit" : #{timeLimit},
"vehicleParams" : #{params1},
"martianParams" : #{params1},
"craters" :  #{objectsToJson(craters)},
"boulders" :  #{objectsToJson(boulders)},
"runs" :  #{runsToJson(runs)}
} 
EOMAP

print marsMap
