import scala.annotation.tailrec
object Practica {
  def main(args: Array[String]): Unit = {
    println(decToBin(1))
    println(decToBin(2))
    println(decToBin(5))
    println(decToBin(10))
    println(decToBin(25))
    println(decToBin(42))
    println()
    println(digitoMayor(58329))
    println(digitoMayor(4441))
    println(digitoMayor(123456))
    println(digitoMayor(80723))
    println(digitoMayor(5))
    println()
    println(contarDigitosPares(583246))
    println(contarDigitosPares(13579))
    println(contarDigitosPares(2468))
    println(contarDigitosPares(102030))
    println(contarDigitosPares(7))
    println()
    println(contarDigito(525235, 5))
    println(contarDigito(11111, 1))
    println(contarDigito(987654, 3))
    println(contarDigito(707070, 0))
    println()
    println(extraerPares(583246))
    println(extraerPares(13579))
    println(extraerPares(2468))
    println(extraerPares(102034))
    println(extraerPares(908172))

    println("\nTAIL RECURSIVE\n")
    println(sumarDigitosPares(583246))
    println(sumarDigitosPares(13579))
    println(sumarDigitosPares(2468))
    println(sumarDigitosPares(102030))
    println(sumarDigitosPares(8))
    println()
    println(contarMayusculas("HolaMundoScala"))
    println(contarMayusculas("SCALA"))
    println(contarMayusculas("programacion"))
    println(contarMayusculas("Scala3EsGenial"))
    println(contarMayusculas(""))
    println()
    println(contarCambiosParidad(123456))
    println(contarCambiosParidad(248135))
    println(contarCambiosParidad(2468))
    println(contarCambiosParidad(13579))
    println(contarCambiosParidad(52841))
    println()
    println(segundoMayor(58329))
    println(segundoMayor(7416))
    println(segundoMayor(9995))
    println(segundoMayor(7777))
    println(segundoMayor(908090))
    println()
    println(grupoMasLargo("aaabbccccdaa"))
    println(grupoMasLargo("aabbbbbcc"))
    println(grupoMasLargo("abcde"))
    println(grupoMasLargo("aaaaaa"))
    println(grupoMasLargo("aabbbaaaa"))
    println(grupoMasLargo(""))
    println(grupoMasLargo("aaAAaa"))
  }

  def decToBin(n: Int): String = {
    if n == 0 then ""
    else decToBin(n / 2) + (if n % 2 == 0 then "0" else "1")
  }

  def digitoMayor(n: Int): Int = {
    if n == 0 then 0
    else {
      val x = digitoMayor(n / 10)
      val d = n % 10
      if x > d then x else d
    }
  }

  def contarDigitosPares(n: Int): Int = {
    if n == 0 then 0
    else contarDigitosPares(n / 10) + (if n % 2 == 0 then 1 else 0)
  }

  def contarDigito(n: Int, x: Int): Int = {
    if n == 0 then 0
    else contarDigito(n / 10, x) + (if n % 10 == x then 1 else 0)
  }

  def extraerPares(n: Int): String = {
    if n == 0 then ""
    else extraerPares(n / 10) + (if n % 2 == 0 then n % 10 else "")
  }

  // Tail Recursive
  @tailrec
  def sumarDigitosPares(n: Int, acc: Int = 0): Int = {
    if n == 0 then acc
    else sumarDigitosPares(n / 10, acc + (if n % 2 == 0 then n % 10 else 0))
  }

  @tailrec
  def contarMayusculas(s: String, acc: Int = 0): Int = {
    if s.isEmpty() then acc
    else
      contarMayusculas(
        s.tail,
        acc + (if s.head != s.head.toLower then 1 else 0)
      )
  }

  def contarCambiosParidad(n: Int): Int = {
    if n == 0 then 0
    else {
      def aux(n: Int, ant: Int, acc: Int): Int = {
        if n <= 10 then math.abs(n % 2 - ant) + acc
        else aux(n / 10, n % 2, acc + math.abs(n % 2 - ant))
      }

      aux(n / 10, n % 2, 0)
    }
  }

  def segundoMayor(n: Int, uno: Int = -1, dos: Int = -1): Int = {
    if n == 0 then dos
    else {
      val digit = n % 10
      if digit >= uno then {
        val dosV2 = if digit > uno then uno else dos
        val unoV2 = digit
        segundoMayor(n / 10, unoV2, dosV2)
      } else if digit > dos then {
        val dosV2 = digit
        segundoMayor(n / 10, uno, dosV2)
      } else segundoMayor(n / 10, uno, dos)
    }
  }

  def grupoMasLargo(
      s: String,
      c: Char = '\u0000',
      acc: Int = 0,
      mayor: Int = -1
  ): Int = {
    if s.length() == 0 then (if acc > mayor then acc else mayor)
    else {
      if s.head != c then
        grupoMasLargo(s.tail, s.head, 1, if acc > mayor then acc else mayor)
      else grupoMasLargo(s.tail, s.head, acc + 1, mayor)
    }
  }

}
