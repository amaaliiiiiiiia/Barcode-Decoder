import Types.{Bit, Digit, Even, Odd, NoParity, One, Parity, Pixel, Str, Zero}
import scala.collection.immutable
import scala.math.Ordering.Implicits.infixOrderingOps

object Decoder {
  // TODO 1.1
  def toBit(s: Char): Bit = {
    if (s == '1') One
    else Zero
  }
  def toBit(s: Int): Bit = {
    if (s == 1) One
    else Zero
  }

  // TODO 1.2
  def complement(c: Bit): Bit =
    if (c == One) Zero
    else One

  // TODO 1.3
  val LStrings: List[String] = List("0001101", "0011001", "0010011", "0111101", "0100011",
    "0110001", "0101111", "0111011", "0110111", "0001011")
  val leftOddList: List[List[Bit]] = {
    LStrings.map(s => s.map(toBit).toList)
  } // codificări L
  val rightList: List[List[Bit]] = {
    leftOddList.map(s => s.map(complement))
  } // codificări R
  val leftEvenList: List[List[Bit]] = {
    rightList.map(s => s.reverse)
  }// codificări  G

  // TODO 1.4
  def group[A](l: List[A]): List[List[A]] = {
    def g(l: List[A], crtL: List[A], group: List[List[A]]): List[List[A]] =
    l match {
      case Nil => (crtL :: group).reverse
      case x :: xs =>
        crtL match {
          case Nil => g(xs, List(x), group) // daca lista curenta este goala
          case y :: ys =>
            if (x == y) g(xs, x :: crtL, group) // daca elementul e egal cu precedentul, il adaug in lista curenta
            else g(xs, List(x), crtL :: group) // daca nu, lista curenta va fi adaugata la lista de liste
        }
    }
    g(l, Nil, Nil)
  }
  
  // TODO 1.5
  def runLength[A](l: List[A]): List[(Int, A)] = {
    def r(list: List[List[A]], newL: List[(Int, A)]): List[(Int, A)] =
      list match {
        case Nil => newL.reverse // daca am ajuns la finalul listei, returnam lista finala (ap, el)
        case x :: xs => r(xs, (x.size, x.head) :: newL) // daca nu, adaugam numarul de aparitii si elementul in noua lista
        // si apelam recursiv pentru restul listei initiale
      }
    r(group(l), Nil)
  }
  
  case class RatioInt(n: Int, d: Int) extends Ordered[RatioInt] {
    require(d != 0, "Denominator cannot be zero")
    private val gcd = BigInt(n).gcd(BigInt(d)).toInt
    val a = n / gcd // numărător
    val b = d / gcd // numitor

    override def toString: String = s"$a/$b"

    override def equals(obj: Any): Boolean = obj match {
      case that: RatioInt => this.a.abs == that.a.abs &&
        this.b.abs == that.b.abs &&
        this.a.sign * this.b.sign == that.a.sign * that.b.sign
      case _ => false
    }

    // TODO 2.1
    def -(other: RatioInt): RatioInt = {
      if (this.b != other.b) {
        val numitor = this.b * other.b
        val numarator = this.a * other.b - other.a * this.b
        val div = BigInt(numarator).gcd(BigInt(numitor)).toInt
        RatioInt(numarator / div, numitor / div)
      }
      else
        RatioInt(this.a - other.a, this.b)
    }
    def +(other: RatioInt): RatioInt = {
      if (this.b != other.b) {
        val numitor = this.b * other.b
        val numarator = this.a * other.b + other.a * this.b
        val div = BigInt(numarator).gcd(BigInt(numitor)).toInt
        RatioInt(numarator / div, numitor / div)
      }
      else
        RatioInt(this.a + other.a, this.b)
    }
    def *(other: RatioInt): RatioInt = {
      val numitor = this.b * other.b
      val numarator = this.a * other.a
      val div = BigInt(numarator).gcd(BigInt(numitor)).toInt
      RatioInt(numarator / div, numitor / div)
    }
    def /(other: RatioInt): RatioInt = {
      val numitor = this.b * other.a
      val numarator = this.a * other.b
      val div = BigInt(numarator).gcd(BigInt(numitor)).toInt
      RatioInt(numarator / div, numitor / div)
    }

    // TODO 2.2
    def compare(other: RatioInt): Int = {
      def greatest(other: RatioInt): Boolean = {
        val numitor = this.b * other.b
        val numarator1 = this.a * other.b // numarator this
        val numarator2 = other.a * this.b // numarator other
        // this este mai mare decat other doar daca numaratorul este mai mare decat al lui other,
        // atunci cand numitorii lor sunt egali si semnul lor sa fie la fel sau semnul lui this sa fie '+',
        //iar al lui other '-'
        if (numarator1 > numarator2 &&
          ((this.a.sign * this.b.sign == other.a.sign * other.b.sign) ||
            (this.a.sign * this.b.sign == '+' && other.a.sign * other.b.sign == '-'))) true
        else false
      }
      if (this.equals(other)) 0
      else if (greatest(other)) 1
      else -1
    }
  }
  
  // TODO 3.1
  def scaleToOne[A](l: List[(Int, A)]): List[(RatioInt, A)] = {
    val nrTotalEl = l.map(_._1).sum
    l.map((x, y) => (RatioInt(x, nrTotalEl), y))
  }

  // TODO 3.2
  def scaledRunLength(l: List[(Int, Bit)]): (Bit, List[RatioInt]) = {
    val firstBit = l.head._2 // bitul primului tuplu din lista
    val nrTotalEl = l.map(_._1).sum
    def scale(l: List[(Int, Bit)], newList: List[RatioInt]): (Bit, List[RatioInt]) = {
      l match {
        case Nil => (firstBit, newList.reverse) // daca s a ajuns la finalul listei, se returneaza perechea de bit si lista
        case (x, b) :: xs => scale(xs, RatioInt(x, nrTotalEl) :: newList) // daca nu, se apeleaza recursiv pentru
        // lista ramasa din cea initiala si lista nou creata cu dimensiunile relative
      }
    }
    scale(l, Nil)
  }
  
  // TODO 3.3
  def toParities(s: Str): List[Parity] = {
    def parity(s: Str, newS: List[Parity]): List[Parity] = {
      s match {
        case Nil => newS.reverse
        case x :: xs =>
          if (x == 'L') parity(xs, Odd :: newS)
          else parity(xs, Even :: newS)
      }
    }
    parity(s, Nil)
  }
  
  // TODO 3.4
  val PStrings: List[String] = List("LLLLLL", "LLGLGG", "LLGGLG", "LLGGGL", "LGLLGG",
    "LGGLLG", "LGGGLL", "LGLGLG", "LGLGGL", "LGGLGL")
  val leftParityList: List[List[Parity]] = {
    PStrings.map(s => toParities(s.toList))
  }

  // TODO 3.5
  type SRL = (Bit, List[RatioInt])
  val leftOddSRL:  List[SRL] = leftOddList.map(l => scaledRunLength(runLength(l)))
  val leftEvenSRL:  List[SRL] = leftEvenList.map(l => scaledRunLength(runLength(l)))
  val rightSRL:  List[SRL] = rightList.map(l => scaledRunLength(runLength(l)))

  // TODO 4.1
  def distance(l1: SRL, l2: SRL): RatioInt = {
    // daca primul bit difera
    if (l1._1 != l2._1)
      return RatioInt(100, 1)

    val l = l1._2.zip(l2._2) // fac tupluri intre lungimile relative
    def dist(l: List[(RatioInt, RatioInt)], acc: RatioInt): RatioInt = {
      l match {
        case Nil => acc
        case x :: xs =>
          dist(xs, acc + RatioInt((x._2 - x._1).a.abs, (x._2 - x._1).b.abs))
      }
    }
    dist(l, RatioInt(0, 1))
  }
  
  // TODO 4.2
  def bestMatch(SRL_Codes: List[SRL], digitCode: SRL): (RatioInt, Digit) = {
    def best(codes: List[SRL], bestM: (RatioInt, Digit), idx: Digit): (RatioInt, Digit) = {
      codes match {
        case Nil => bestM
        case x :: xs =>
          val d = distance(x, digitCode)
          if (d.compare(RatioInt(0, 1)) == 0) {
              return (d, idx)
          }
          val newBest = if (d.compare(bestM._1) < 0) (d, idx) else bestM
          best(xs, newBest, idx + 1)
      }
    }
    best(SRL_Codes, (RatioInt(100, 1), -1), 0)
  }
  
  // TODO 4.3
  def bestLeft(digitCode: SRL): (Parity, Digit) = {
    val bestO = bestMatch(leftOddSRL, digitCode)
    val bestE = bestMatch(leftEvenSRL, digitCode)
    if (bestO._1.compare(RatioInt(0, 1)) == 0)
      (Odd, bestO._2)
    else if (bestE._1.compare(RatioInt(0, 1)) == 0)
      (Even, bestE._2)
    else if (bestO._1.compare(bestE._1) == -1)
      (Odd, bestO._2)
    else
      (Even, bestE._2)
  }
  
  // TODO 4.4
  def bestRight(digitCode: SRL): (Parity, Digit) = {
    val bestR = bestMatch(rightSRL, digitCode)
    (NoParity, bestR._2)
  }

  def chunkWith[A](f: List[A] => (List[A], List[A]))(l: List[A]): List[List[A]] = {
    l match {
      case Nil => Nil
      case _ =>
        val (h, t) = f(l)
        h :: chunkWith(f)(t)
    }
  }
  
  def chunksOf[A](n: Int)(l: List[A]): List[List[A]] =
    chunkWith((l: List[A]) => l.splitAt(n))(l)

  // TODO 4.5
  def findLast12Digits(rle:  List[(Int, Bit)]): List[(Parity, Digit)] = {
    val rleWithoutStart = rle.drop(3) // elimin 3 biti de start
    val rleWithoutStop = rleWithoutStart.dropRight(3) // elimin 3 biti de stop
    val (left, right) = rleWithoutStop.splitAt(24) // impart in cate 24 de bare

    val leftChunks = chunksOf(4)(left) // impart stanga in cate 4 bare
    val rightChunks = chunksOf(4)(right.drop(5)) // elimin cei 5 biti de mijloc si impart dreapta in cate 4 bare

    val leftDigits = leftChunks.map(group => bestLeft(scaledRunLength(group)))
    val rightDigits = rightChunks.map(group => bestRight(scaledRunLength(group)))

    leftDigits ++ rightDigits
  }

  // TODO 4.6
  def firstDigit(l: List[(Parity, Digit)]): Option[Digit] = {
    val leftParity = leftParityList.zipWithIndex // fiecarei liste din lista leftParityList i am asociat un index
    val list = l.take(6).map(_._1) // paritatile grupului din stanga

    def count(left: List[(List[Parity], Int)]): Option[Digit] = {
      left match {
        case Nil => None
        case x :: xs =>
          if (x._1 == list)
            Some(x._2)
          else
            count(xs)
      }
    }
    count(leftParity)
  }

  // TODO 4.7
  def checkDigit(l: List[Digit]): Digit = {
    val digits = l.take(12) // primele 12 cifre
    def sum(d: List[Digit], acc: Digit, index: Int): Digit = {
      d match {
        case Nil => acc
        case x :: xs =>
          if (index % 2 == 1) sum(xs, acc + x * 1, index + 1)
          else sum(xs, acc + x * 3, index + 1)
      }
    }
    val suma = sum(digits, 0, 1)
    (10 - (suma % 10)) % 10
  }
  
  // TODO 4.8
  def verifyCode(code: List[(Parity, Digit)]): Option[String] = {
    if (code.length == 13) {
      val first = firstDigit(code.takeRight(12))
      if (first == Some(code.head._2)) {
        val check = checkDigit(code.take(12).map(_._2))
        if (check == code.last._2) {
          Some(code.map(_._2).mkString)
        } else {
          None
        }
      } else {
        None
      }
    } else {
      None
    }
  }
  
  // TODO 4.9
  def solve(rle:  List[(Int, Bit)]): Option[String] = {
    val result = findLast12Digits(rle)
    val first = firstDigit(result)

    first match {
      case Some(firstDigit) =>
        val whole = (NoParity, firstDigit) :: result
        
        verifyCode(whole) match {
          case Some(code) => Some(code)
          case None => None
        }

      case None => None
    }
  }
  
  def checkRow(row: List[Pixel]): List[List[(Int, Bit)]] = {
    val rle = runLength(row);

    def condition(sl: List[(Int, Pixel)]): Boolean = {
      if (sl.isEmpty) false
      else if (sl.size < 59) false
      else sl.head._2 == 1 &&
        sl.head._1 == sl.drop(2).head._1 &&
        sl.drop(56).head._1 == sl.drop(58).head._1
    }

    rle.sliding(59, 1)
      .filter(condition)
      .toList
      .map(_.map(pair => (pair._1, toBit(pair._2))))
  }
}


