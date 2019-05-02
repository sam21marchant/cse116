import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.io.{IO, Tcp}
import akka.util.ByteString

/////// Q1 (30 points): Study the following code to answer the question below //////////////////


case class MessageType(value: Int)

class QuizActor(next: ActorRef) extends Actor {
  def receive: Receive = {
    case message: MessageType =>
      if (message.value < 10) {
        next ! MessageType(message.value - 1)
      } else {
        next ! MessageType(message.value / 2)
      }
  }
}

class QuizActor2() extends Actor {
  def receive: Receive = {
    case message: MessageType =>
      println(message.value)
  }
}


object Q1 {

  def main(args: Array[String]): Unit = {
    val system = ActorSystem("QuizSystem")

    val mainActor = system.actorOf(Props(classOf[QuizActor2]))

    val first = system.actorOf(Props(classOf[QuizActor], mainActor))
    val second = system.actorOf(Props(classOf[QuizActor], first))
    val third = system.actorOf(Props(classOf[QuizActor], second))

    val extra = system.actorOf(Props(classOf[QuizActor], second))


    third ! MessageType(10)
    extra ! MessageType(5)
  }
}

/**
  * What two values are printed when this program runs?
  */

/////// Q2 (30 points) Study the following code below. You may assume all of the necessary imports are included


///// Server /////

class TCPServer() extends Actor {

  import akka.io.Tcp._
  import context.system

  IO(Tcp) ! Bind(self, new InetSocketAddress("localhost", 8000))

var buffer: String = ""
val delimiter: String = "~"

override def receive: Receive = {
  case b: Bound => println("Listening on port: " + b.localAddress.getPort)
  case c: Connected =>
    println("connected")
    sender() ! Register(self)
    sender() ! Write(ByteString("Hello!" + delimiter))
  case r: Received =>
    buffer += r.data.utf8String
    while (buffer.contains(delimiter)) {
      val message = buffer.substring(0, buffer.indexOf(delimiter))
      buffer = buffer.substring(buffer.indexOf(delimiter) + 1)
      if(message.contains("Are you ok?!")){
        sender() ! Write(ByteString("Yes!" + delimiter))
      } else {
        sender() ! Write(ByteString("ACK" + delimiter))
      }
    }
}
}

object Q2 {
  def main(args: Array[String]): Unit = {
    val actorSystem = ActorSystem()
    actorSystem.actorOf(Props(classOf[TCPServer]))
  }
}



/////// Client ///////
scala_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
scala_socket.connect(('localhost', 8000))

delimiter = "~"
number = [0]


def listen_to_scala(the_socket):
    buffer = ""
    while True:

        while delimiter in buffer:
            message = buffer[:buffer.find(delimiter)]
            buffer = buffer[buffer.find(delimiter) + 1:]
            get_from_scala(message)


def get_from_scala(message):
    if "Hello" in message:
        send_to_scala("Hello Scala!")
    elif "ACK" in message:
        send_to_scala("Are you ok?!")
        number[0] -= len(message)
    else:
        number[0] += len(message)
        print(number[0])


def send_to_scala(data):
    scala_socket.sendall((json.dumps(data) + delimiter).encode())


send_to_scala("Hello Scala!")
listen_to_scala(scala_socket)


/**
  * What is the final value of number[0]?
  */



////// Question 3 (40 Points) Study the following code containing a subset of the Clicker 2 functionality

/// Server .py

from flask import Flask, request, send_from_directory
from flask_socketio import SocketIO

app = Flask(__name__)
socket_server = SocketIO(app)

sidToUsername = {}
clicks = {}


@socket_server.on('register')
def register(username):
    sidToUsername[request.sid] = username
    if username not in clicks:
        clicks[username] = 0
    socket_server.emit("message", str(clicks[username]), room=request.sid)
    print(username + " connected")


@socket_server.on('disconnect')
def disconnect():
    if request.sid in sidToUsername:
        username = sidToUsername[request.sid]
        del sidToUsername[request.sid]
        print(username + " disconnected")


@socket_server.on('clickGold')
def click_gold():
    username = sidToUsername[request.sid]
    clicks[username] += 1
    socket_server.emit("message", str(clicks[username]), broadcast=True)
    print(clicks)


@app.route('/')
def index():
    return send_from_directory('.', 'game.html')


@app.route('/<path:filename>')
def static_files(filename):
    return send_from_directory('.', filename)


print("Listening on port 8080")
socket_server.run(app, port=8080)


///// game.html ////////////////////////////////////////////////////////////////////////////////////////////////
<!DOCTYPE html>
  <html lang="en">
    <head>
      <meta charset="UTF-8">
        <title>Clicker Quiz</title>
        <script type="text/javascript" src="https://cdnjs.cloudflare.com/ajax/libs/socket.io/2.2.0/socket.io.js"></script>
    </head>
    <body>

      <button id="gold" onclick="clickGold();">GOLD!</button>
      <div id="displayGold"></div>
      <script src="game.js"></script>

      </body>
  </html>


///// game.js /////////////////////////////////////////////////////////////////////////////////////////////////////
var socket = io.connect({transports: ['websocket']});

socket.on('message', function (event) {
  document.getElementById("displayGold").innerHTML = event;
});

socket.emit("register", "ScalaUser");

function clickGold(){
  socket.emit("clickGold");
}



///// ScalaClient.scala ////////////////////////////////////////////////////////////////////////////


import io.socket.client.{IO, Socket}
import io.socket.emitter.Emitter


class HandleMessagesFromPython() extends Emitter.Listener {
override def call(objects: Object*): Unit = {
  val gold = objects.apply(0).toString
  println("I have " + gold + " gold")
}
}


object ScalaClient {

def main(args: Array[String]): Unit = {

  val socket: Socket = IO.socket("http://localhost:8080/")
  socket.on("message", new HandleMessagesFromPython)

  socket.connect()
  socket.emit("register", "ScalaUser")

  socket.emit("clickGold")
  socket.emit("clickGold")
  socket.emit("clickGold")
  socket.emit("clickGold")
  socket.emit("clickGold")
}

}


/**
When this code is working properly it should have the following features:

*When server.py is ran it hosts game.html/js and listens for websocket connections on port 8080

*When a user sends a register message to the python server with a username it will associate this username with their socket id 
 and setup a data structure to remember the number of clicks they’ve made

*If a username registers again they continue with the same number of clicks they’ve had (The server effectively saves their game, 
 though it does not use persistent storage so saved games are lost when the server restarts)

*When a web client connects they see a page with a gold button and a display of their current gold (number of times they’ve 
 clicked the button). As they click, the number is incremented. If they reconnect they see their total number of clicks across 
 all connections

*Each time the scala client is ran it will simulate 5 clicks of a gold button and print out the total clicks from Scala after 
 each click

*/

//The code provided does not fully realize all of these features. There are 2 bugs in the code. Find each bug, 
  describe which feature is broken by the bug, and explain why the feature is broken, and how you’d fix it

///// Bug # 1:
      Where is the bug?
      What does this break?
      How is the feature broken?
      What would you do to fix the bug?

///// Bug # 1:
      Where is the bug?
      What does this break?
      How is the feature broken?
      What would you do to fix the bug?
