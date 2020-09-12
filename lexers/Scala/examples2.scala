import Pattern._
import Family._

def pattern[B](pf: PartialFunction[Name,B]) = new Extractor(pf.lift)

val Parents = new Extractor(parents.get)
val Children = new Extractor(children.get)

"Julie" match {
  case Parents(p) => "Julies parents are: " + p
  case Children(c) => "Julies parents are unknown but has children: " + c
  case _ => "Don't know any of Julie's relatives"
}

parents.get("Julie") map { p => "Julies parents are: " + p } getOrElse { 
  children.get("Julie") map { c => "Julies parents are unknown but has children: " + c } getOrElse { 
    "Don't know any of Julie's relatives" 
  }
}

"Nalda" match { 
  case Children(Children(c)) => "Nalda's grandchildren are: " + c 
  case Children(_) => "Nalda has children but no grandchildren"
  case _ => "Nalda is childless"
}

val Female = pattern { case n if female contains n => n }

"Nalda" match { 
  case Children(Children(Female(d))) => "Nalda's granddaughters are: " + d 
  case Children(Children(_)) => "Nalda has grandchildren but no granddaughters"
  case _ => "Nalda has no grandchildren"
}

val GrandChildren = pattern { case Children(Children(c)) => c }
val GrandDaughters = pattern { case GrandChildren(Female(c)) => c }


val Mother = pattern { case Parents(Female(p)) => p }
val Siblings = pattern { case self @ Parents(Children(siblings)) => siblings - self }
val Sisters = pattern { case Siblings(Female(s)) => s }
val Male = pattern { case n if male contains n => n }
val Brothers = pattern { case Siblings(Male(b)) => b }

"Julie" match {
  case Brothers(_) & Sisters(_) => "Julie has both brother(s) and sister(s)"
  case Siblings(_) => "Julie's siblings are all the same sex"
  case _ => "Julie has no siblings"
}
