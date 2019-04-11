
//Q1 (25 points): Study the following code to answer the question below

class User(var name: String, var score: Int) {
  override def toString: String = name + ": " + score
}

object Q1_Runner {

  def sortTeam(): Unit = {
    var teamMates = List(
      new User("Anakin", 5),
      new User("Indiana Jones", 8),
      new User("Thanos", 3),
      new User("Jesse", 20),
      new User("Carl", 20)
    )

    val comparator: (User, User) => Boolean =
      (a: User, b: User) => {
        if (a.score > b.score) {
          true
        } else if (a.score < b.score) {
          false
        } else {
          if (a.name < b.name) {
            true
          } else {
            false
          }
        }
      }

    teamMates = teamMates.sortWith(comparator)

    println(teamMates)
  }

  def main(args: Array[String]): Unit = {
    sortTeam()
  }
}

/*****
  In what order are the elements of the inventory list printed at the end of the sortTeam() method call?
  *******/


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

//Q2 (25 points): Study the following code to answer the question below

object Q2_Runner {
  def f(x: Int, y: Int): Int = {
    if (y <= 0) {
      1
    } else {
      x * x  * f(x, y - 2)
    }
  }

  def main(args: Array[String]): Unit = {
    println(f(3, 3))
  }
}


 /**
  * What is printed after the Q2_Runner main method is done executing?
  */

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


//Q3 (25 points): Study the following code to answer the question below


object Q3_Runner{
  def f(n: Int): Int = {
    if (n <= 0) {
      0
    } else if (n == 1){
      1
    }
    else {
      f(n - 1) + f(n - 2)
    }
  }


  def main(args: Array[String]): Unit = {
    println(f(5))
  }
}

/**
  * What is printed after the main method in Q3_Runner is done executing?
  */


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


//Q4 (25 points): Study the following code to answer the question below


class User(var name: String, var score: Int) {
  override def toString: String = name + ": " + score
}

class LinkedListNode[A](var value: A, var next: LinkedListNode[A]) {

  def foo(f: A => A): String = {
    // Feel free to change this method away from this structure
    if (this.next == null) {
      this.value = f(this.value)
      this.value.toString
    }else{
      this.value = f(this.value)
      this.value.toString + "\n" + this.next.foo(f)
    }
  }

  def prepend(a: A): LinkedListNode[A] = {
    new LinkedListNode[A](a, this)
  }

  def map[B](f: A => B): LinkedListNode[B] = {
    val newValue = f(this.value)
    if (this.next == null) {
      new LinkedListNode[B](newValue, null)
    } else {
      new LinkedListNode[B](newValue, this.next.map(f))
    }
  }

  def foreach(f: A => Unit): Unit = {
    f(this.value)
    if (this.next != null) {
      this.next.foreach(f)
    }
  }

}


object Q4_Runner{

  def q4(): String = {
    var myList: LinkedListNode[User] = new LinkedListNode[User](
      new User("Mickey", 12), null
    )
    myList = myList.prepend(new User("Minnie", 19))
    myList = myList.prepend(new User("Goofy", 8))
    myList = myList.prepend(new User("Pluto", 11))
    myList = myList.prepend(new User("Donald Duck", 5))
    myList = myList.prepend(new User("Steamboat Willy", 21))


    val f: User => Unit = (x: User) => x.score += 10
    myList.foreach(f)


    val g: User => User = (instance: User) => {
      instance.name = instance.name.replace("a", "")
      instance.name = instance.name.replace("e", "")
      instance.name = instance.name.replace("i", "")
      instance.name = instance.name.replace("o", "")
      instance.name = instance.name.replace("u", "")
      instance.name = instance.name.replace("y", "")
      instance
    }

    myList.foo(g)

  }

  def main(args: Array[String]): Unit = {
    println(q4())
  }

}

/**
  * What is printed after the main method in Q4_Runner is done executing?
  */
