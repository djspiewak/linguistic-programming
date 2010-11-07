import scala.collection.mutable
import scala.util.continuations._

object Baysick {
  val running = new ThreadLocal[Boolean] {
    override val initialValue = true
  }
  
  val env = new ThreadLocal[mutable.Map[Symbol, Any]] {
    override val initialValue = mutable.Map[Symbol, Any]()
  }
  
  val code = new ThreadLocal[mutable.Map[Int, Unit => Fragment]] {
    override val initialValue = mutable.Map[Int, Unit => Fragment]()
  }
  
  val whilst = new ThreadLocal[mutable.Stack[Int]] {
    override val initialValue = mutable.Stack[Int]()
  }
  
  val wends = new ThreadLocal[mutable.Map[Int, Int]] {
    override val initialValue = mutable.Map[Int, Int]()
  }
  
  val whilting = new ThreadLocal[Boolean] {
    override val initialValue = false
  }
  
  
  sealed trait Fragment
  case class Continue(lineNum: Int, k: Unit => Fragment) extends Fragment
  case object Halt extends Fragment
  
  sealed trait Expr {
    def eval: Any
    
    def <(that: Expr) = LtExpr(this, that)
    
    def =:=(that: Expr) = EqExpr(this, that)
    
    def *(that: Expr) = MulExpr(this, that)
    
    def +(that: Expr) = AddExpr(this, that)
  }
  
  case class StrExpr(eval: String) extends Expr
  
  case class IntExpr(eval: Int) extends Expr
  
  case class VarExpr(sym: Symbol) extends Expr {
    def eval = env.get()(sym)
  }
  
  case class LtExpr(left: Expr, right: Expr) extends Expr {
    def eval = {
      val li = left.eval.asInstanceOf[Int]
      val ri = right.eval.asInstanceOf[Int]
      li < ri
    }
  }
  
  case class EqExpr(left: Expr, right: Expr) extends Expr {
    def eval = left.eval == right.eval
  }
  
  case class MulExpr(left: Expr, right: Expr) extends Expr {
    def eval = left.eval.asInstanceOf[Int] * right.eval.asInstanceOf[Int]
  }
  
  case class AddExpr(left: Expr, right: Expr) extends Expr {
    def eval = left.eval.asInstanceOf[Int] + right.eval.asInstanceOf[Int]
  }
  
  
  class Commands(lineNum: Int) {
    def PRINT(e: Expr) = {
      preserve(lineNum)
      println(e.eval.toString)
    }
    
    def LET(sym: Symbol) = {
      preserve(lineNum)
      new {
        def :=(expr: Expr) {
          env.get += (sym -> expr.eval)
        }
      }
    }
    
    def WHILE(predicate: Expr) = {
      preserve(lineNum)
      val result = predicate.eval.asInstanceOf[Boolean]
      
      if (result) {
        preserve(-1)
        whilst.get.push(lineNum)
      } else {
        whilting.set(true)
        GOTO(wends.get()(lineNum))
      }
    }
    
    def WEND() = {
      preserve(lineNum)
      if (whilting.get) {
        preserve(-1)
        whilting.set(false)
      } else {
        val target = whilst.get.pop
        wends.get += (target -> lineNum)
        GOTO(target)
      }
    }
    
    def GOTO(target: Int) = {
      preserve(lineNum)
      handle(code.get()(target)(()))
    }
    
    def IF(predicate: Expr) = {
      preserve(lineNum)
      val result = predicate.eval.asInstanceOf[Boolean]
      
      if (result) {
        new {
          def THEN(target: Int) = {
            GOTO(target)
          }
        }
      } else {
        new {
          def THEN(target: Int) = {
            preserve(-1)
          }
        }
      }
    }
    
    def INPUT(sym: Symbol) = {
      preserve(lineNum)
      env.get += (sym -> readLine())
    }
  }
  
  
  def baysick(body: =>(Unit @cps[Fragment])) {
    val result = reset {
      body
      Halt: Fragment
    }
    
    handle(result)
  }
  
  private def run(k: Unit => Fragment) {
    if (running.get) {
      handle(k(()))
    }
  }
    
  private def handle(f: Fragment) = f match {
    case Continue(lineNum, k) => {
      if (!code.get.contains(lineNum)) {
        code.get += (lineNum -> k)
      }
      run(k)
    }
    
    case Halt => running.set(false)
  }
  
  private def preserve(lineNum: Int) = {
    // preserve line for later goto
    // note: cannot do forward jumps!
    shift { (k: Unit => Fragment) => Continue(lineNum, k) }
  }
  
  
  implicit def strToExpr(str: String) = StrExpr(str)
  
  implicit def intToExpr(i: Int) = IntExpr(i)
  
  implicit def symToVar(sym: Symbol) = VarExpr(sym)
  
  implicit def intToCommands(lineNum: Int) = new Commands(lineNum)
}

// ...

import Baysick._

println("Father forgive me, for I have sinned...")

baysick {
  10  PRINT "Printing the multiples of 2 < 20"
  
  20  LET 'i := 1
  30  WHILE 'i < 10
  40  LET 'res := 'i * 2
  50  PRINT 'res
  60  LET 'i := 'i + 1
  70  WEND
  
  80  PRINT "Go again?"
  90  INPUT 'again
  100 IF 'again =:= "y" THEN 10
  
  110 PRINT "Goodbye!"
}