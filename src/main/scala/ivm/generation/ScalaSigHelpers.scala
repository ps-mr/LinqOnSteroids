package ivm
package generation

import tools.scalap.scalax.rules.scalasig
import scalasig._
//{ScalaSigParser,Symbol,ClassSymbol,MethodSymbol,ScalaSig,NullaryMethodType, PolyType, MethodType, ScalaSigPrinter}
import tools.scalap.scalax.util.StringUtil
import collection.mutable.ListBuffer
import collection.mutable.Set
import java.lang.{ClassLoader,Thread,Class}
import java.net.URL
import java.util.regex.Pattern
import java.util.zip.{ZipInputStream,ZipEntry}
import java.io.{PrintStream}
import scala.tools.scalap.scalax.util.StringUtil

object ScalaSigHelpers {

  val WRAP_BEGIN = "Exp["
  val WRAP_END   = "]"

  val sigp = new ScalaSigPrinter(null,true)
  import sigp._tf

  def getScalaSig(className: String) = ScalaSigParser.parse(Class.forName(className)).get

  def getMethodSymbols(symbol: ClassSymbol, sig: ScalaSig, fil: MethodSymbol => Boolean): Seq[MethodSymbol] = {
    (symbol.children filter (s => s.isMethod && fil(s.asInstanceOf[MethodSymbol]))).map (s => s match {
      case MethodSymbol(_, _) => s.asInstanceOf[MethodSymbol]
    })
  }

  def getClassesInPackage(packageName: String, filter: String => Boolean = (_ => true)): Seq[String] = {
    val classLoader = Thread.currentThread().getContextClassLoader()
    val path = packageName.replace('.', '/')
    val resources = classLoader.getResources(path)
    val dirs = ListBuffer.empty[String]
    while (resources.hasMoreElements)
      dirs += resources.nextElement.getFile
    dirs flatMap (dir => findClasses(dir, packageName, filter))
  }

  def findClasses(path: String, packageName: String, filter: String => Boolean): Set[String] = {
    val classes = Set.empty[String]
    if (path.startsWith("file:") && path.contains("!")) {
      val jar = new URL(path.split("!")(0))
      val zip = new ZipInputStream(jar.openStream())
      var entry = zip.getNextEntry

      while (entry != null) {
	if (!entry.isDirectory && entry.getName.endsWith(".class")) {
	  // filter the classes in the right directory (e.g. right package)
	  // and filter with regex pattern
	  
	  val className = entry.getName.replaceAll("[.]class", "").replace('/', '.')
	  if (className.startsWith(packageName) && filter(className))
	    classes += className
	}

	entry = zip.getNextEntry
      }
    }
    classes
  }

  val classFilter: String => Boolean = (s => !s.contains("$"))
  


  def getModifiers(symbol: Symbol): List[String] = {
    val buffer = new ListBuffer[String]
    if (symbol.isPrivate) buffer += "private"
    else if (symbol.isProtected) buffer += "protected"
    else symbol match {
      case sym: SymbolInfoSymbol => sym.symbolInfo.privateWithin match {
	case Some(t: Symbol) => buffer += ("private[" + t.name + "]")
	case _ =>
      }
      case _ =>
    }

    if (symbol.isSealed) buffer += "sealed"
    if (symbol.isImplicit) buffer += "implicit"
    if (symbol.isFinal && !symbol.isInstanceOf[ObjectSymbol]) buffer += "final"
    if (symbol.isOverride) buffer += "override"
    if (symbol.isAbstract) symbol match {
      case c@(_: ClassSymbol | _: ObjectSymbol) if !c.isTrait => buffer += "abstract"
      case _ =>
    }
    if (symbol.isCase && !symbol.isMethod) buffer += "case"
    return buffer.toList
  }

  def getResultType(ms: MethodSymbol, wrapped: Boolean = true): String = {
    val t = ms.infoType
    t match {
      case PolyType(mt, _) => if (wrapped) getWrappedType(mt, Map("scala.Array" -> "scala.Seq")) else getType(mt)
      case _               => if (wrapped) getWrappedType(t, Map("scala.Array" -> "scala.Seq"))  else getType(t)
    }
  }

  def getClassifier(ms: MethodSymbol): String = {
    if (ms.isAccessor) {
      val indexOfSetter = ms.parent.get.children.indexWhere(
	x => x.isInstanceOf[MethodSymbol] &&
	x.asInstanceOf[MethodSymbol].name == ms.name + "_$eq")
      if (indexOfSetter > 0) return "var" else "val"
    } else return "def"
  }

  def getTypeParameters(ms: MethodSymbol): String = {
    ms.infoType match {
      case PolyType(_, typeParams) => sigp.typeParamString(typeParams)
      case _                       => ""
    }
  }

  def getNumberOfParameters(ms: MethodSymbol, all: Boolean = false): Int = {
    if (all) {
      (getParameters(ms) map (_._2.length)).sum
    } else 
      getParameters(ms).headOption match {
	case Some(x) => x._2.length
	case _       => 0
      }
  }

  def getParametersAsString(ms: MethodSymbol, wrapped: Boolean = true): String = {
    val p = getParameters(ms, wrapped)
    if (!p.isEmpty) {
      (p map (x => {
	val START = if (x._1) "(implicit " else "("
	(x._2 map (y => y._1 + ": " + y._2)).mkString(START, ",", ")")
      })).fold("")(_ ++ _)
    }
    else ""
  }

  def getParameterTypesAsString(ms: MethodSymbol, prefix: String = "[", suffix: String = "]", wrapped: Boolean = true, transform: String => String = x => x): String = {
    getPartOfParameters(ms, prefix, suffix, wrapped, x => x._2, transform)
  }

  def getParameterNamesAsString(ms: MethodSymbol, prefix: String = "(", suffix: String = ")", wrapped: Boolean = true, transform: String => String = x => x): String = {
    getPartOfParameters(ms, prefix, suffix, wrapped, x => x._1, transform)
  }

  def toUnderscore: String => String = _ => "_"

  def getPartOfParameters(ms: MethodSymbol, prefix: String, suffix: String, wrapped: Boolean, select: Pair[String, String] => String, transform: String => String): String = {
    val p = getParameters(ms, wrapped)
    if (!p.isEmpty) {
      (p map (x => { if (!x._2.isEmpty)
		    (x._2 map (y => transform(select(y)))).mkString(prefix, ",", suffix)
		     else ""
      })).fold("")(_ ++ _)
    }
    else ""
  }

  def getParameters(ms: MethodSymbol, wrapped: Boolean = true): List[(Boolean,List[(String,String)])] = getParameters(ms.infoType, wrapped)

  def getParameters(methodType: Type, wrapped: Boolean): List[(Boolean,List[(String,String)])] = {
    methodType match {
      case MethodType(resType, paramSymbols) => {
	val implicitWord: Boolean = paramSymbols.headOption match {
	  case Some(p) if p.isImplicit => true
	  case _                       => false
	}
	List((implicitWord,
	 (paramSymbols.map(s => {
	   val pname = s.name
	   val ptype = s match {
	     case ms: MethodSymbol => if (wrapped) getWrappedType(ms.infoType)
				      else getType(ms.infoType)
	     case _                => "^___^"
	   }
	   (pname, ptype)
	 }).toList))) ++ getParameters(resType, wrapped)
      }
      case PolyType(mt,_) => getParameters(mt, wrapped)
      case _ => List()
    }
  }

  def getType(ms: MethodSymbol, wrapped: Boolean = true)(implicit flags: sigp.TypeFlags): String = {
    if (wrapped) getWrappedType(ms.infoType)(flags)
    else getType(ms.infoType)(flags)
  }

  def getType(t: Type)(implicit flags: sigp.TypeFlags): String = {
    sigp.toString(t)(flags)
  }

  def getWrappedType(t: Type, m: Map[String,String] = Map.empty)(implicit flags: sigp.TypeFlags): String = {
    t match {
      case ThisType(symbol) => WRAP_BEGIN + sigp.processName(symbol.path) + ".type" + WRAP_END
      case SingleType(typeRef, symbol) => WRAP_BEGIN + sigp.processName(symbol.path) + ".type" + WRAP_END
      
      case c@ConstantType(constant) => WRAP_BEGIN + getType(c)(flags) + WRAP_END
      case TypeRefType(prefix, symbol, typeArgs) => (symbol.path match {
	case "scala.<repeated>" => flags match {
	  case sigp.TypeFlags(true) => getWrappedType(typeArgs.head, m) + "*"
	  case _ => WRAP_BEGIN + "scala.Seq" + sigp.typeArgString(typeArgs) + WRAP_END
	}
	case "scala.<byname>" => "=> " + getWrappedType(typeArgs.head, m)
	case x => {
	  if (m.contains(x)) {
	    WRAP_BEGIN + StringUtil.trimStart(m(x) + sigp.typeArgString(typeArgs), "<empty>.") + WRAP_END
	  }
	  else {
	    val path = StringUtil.cutSubstring(symbol.path)(".package")
	    if (symbol.path.matches("scala.Function\\d")) {
	      StringUtil.trimStart(sigp.processName(path) + typeArgStringWrapped(typeArgs), "<empty>.")
	    } else {
	      WRAP_BEGIN + StringUtil.trimStart(sigp.processName(path) + sigp.typeArgString(typeArgs), "<empty>.") + WRAP_END
	    }
	  }
	}
      })

      case MethodType(resultType, _) => getWrappedType(resultType, m)(flags)
      case NullaryMethodType(resultType) => getWrappedType(resultType, m)(flags)
      case PolyType(typeRef, symbols) => sigp.typeParamString(symbols) + getWrappedType(typeRef, m)(flags)

      case _ => getType(t)(flags)
    }
  }
  def typeArgStringWrapped(typeArgs: Seq[Type]): String =
    if (typeArgs.isEmpty) ""
    else typeArgs.map(getWrappedType(_)).map(StringUtil.trimStart(_, "=> ")).mkString("[", ", ", "]")

}
