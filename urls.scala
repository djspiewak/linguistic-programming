import scala.xml._

object Web {
  case class URLSpec(paths: Vector[String], params: Option[Seq[(String, String)]]) {
    def ?(params: (String, String)*) = URLSpec(paths, Some(params))
    
    def /(that: URLSpec) = {
      val paths2 = this.paths ++ that.paths
      val params2 = this.params map { _ ++ (that.params getOrElse Nil) } orElse that.params
      URLSpec(paths2, params2)
    }
    
    def /(path: String) = URLSpec(paths :+ path, params)
  }
  
  case class Protocol(name: String) {
    def :/(spec: URLSpec) = URL(name, spec)
  }
  
  case class URL(protocol: String, spec: URLSpec) {
    def get = NodeSeq.Empty        // TODO
    
    def post(params: (String, String)*) = NodeSeq.Empty     // TODO
  }
  
  val http = Protocol("http")
  val https = Protocol("https")
  
  implicit def strToSpec(str: String) = URLSpec(Vector(str), None)
}


// ...

import Web._

val blog = http :/ "www.codecommit.com" / "blog"
blog.get      // => XML fragment

val search = http :/ "www.google.com" / "search" ? ("q" -> "Daniel Spiewak")
search.get      // => XML fragment

https :/ "www.banksimple.com"

val twitter = http :/ "twitter.com" / "login"
twitter.post("username" -> "djspiewak", "password" -> "n0tRLY4p@sswoRd")
