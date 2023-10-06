import akka.actor.Actor
import akka.actor.ActorSystem

object es3 {
    class SimpleActor extends Actor {
        def receive = {
            case s : String => println("string: "+ s)
            case i : Int => println("number : "+i)
        }
        def foo = println("normal method")
    }
    val system = ActorSystem("SimpleSystem")
    val actor = system.actorOf(Props[SimpleActor],"SimpleActor")
}