import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import scala.io.Source
import org.apache.spark.rdd.RDD
import org.tartarus.snowball.ext.russianStemmer

object lab2 {

  val NODES = 3
  val IN = "D:\\var_09.txt"
  def main(args: Array[String]) {
    val conf = new SparkConf()
      .setAppName("TestLab")
      .setMaster(s"local[$NODES]")
    val sc = new SparkContext(conf)
    val input = sc
      .textFile(IN)
      .repartition(NODES)
    val count = input.count()
    println("Count strok in file " + count)
    val clean = CleanText(input)
    val top = Top50(clean)
    val antitop = AntiTop50(clean)
    val rdd = TextClean(input)
    val rdd3 = RefText(rdd)
    val top2 = Top(rdd3)
    val antitop3 = AntiTop(rdd3)

    sc.stop()
  }

  //Clean Text
  def CleanText(clean_text: RDD[String]): RDD[String] = {
    val clear_text1 = clean_text
      .map(x => x.replaceAll("(?:(?![_-])\\p{Punct})+", ""))
    val stopWords = Source
      .fromFile("stoplist.txt").getLines().toList
    val clear_text2 = clear_text1
      .flatMap(x => x.toLowerCase().split(" "))
      .filter(!stopWords.contains(_))
    val Clean_Text = clear_text2
      .filter(x => x.size > 2)
    return Clean_Text
  }
  //TOP-50

  def Top50(top: RDD[String]): Array[(String, Int)] = {
    val counts = top
      .flatMap(line => line.split(" "))
      .map(word => (word, 1))
      .reduceByKey(_ + _)
    val c = counts.collect()
    println("Word Count " + counts.count())
   // println("TOP-50")
    val sort = counts
      .sortBy(-_._2).take(50)
     println("TOP-50") 
    sort.foreach(println)
    return sort
  }
  //ANTITOP-50
  
  def AntiTop50(top: RDD[String]): Array[(String, Int)] = {
    val counts = top
      .flatMap(line => line.split(" "))
      .map(word => (word, 1))
      .reduceByKey(_ + _)
    val sort = counts
      .sortBy(_._2).take(50)
      println("ANTITOP-50")
    sort.foreach(println)
    return sort
  }
  //Stemming words
  def stemming(s: String): String = {
    val stemmer = new russianStemmer()
    stemmer.setCurrent(s)
    stemmer.stem()
    val str = stemmer.getCurrent()
    return str
  }

  //Clean Text
  def TextClean(clean_text: RDD[String]): RDD[String] = {
    val clear_text1 = clean_text
      .map(x => x.replaceAll("""[\p{Punct}&&[^-]]""", " "))
    val stopWords = Source
      .fromFile("stoplist.txt")
      .getLines().toList
    val clear_text2 = clear_text1
      .flatMap(x => x.toLowerCase().split(" "))
      .filter(!stopWords.contains(_))
    val Clean_Text = clear_text2
      .filter(x => x.size > 2)
    return Clean_Text
  }

  //TOP-50
  def Top(top50_rdd: RDD[(Int, (String, Iterable[(String, Int)]))]): Array[(Int, (String, Iterable[(String, Int)]))] = {
    val top_50 = top50_rdd
      .sortBy(-_._1).take(50)
      println("TOP-50 stemming")
    top_50.foreach(x => println(s"${x._1}: ${x._2._1}: ${x._2._2.map(y => y._1 + "-" + y._2).mkString(" ")}"))
    return top_50
  }

  //AntiTop-50
  def AntiTop(anti_top50_rdd: RDD[(Int, (String, Iterable[(String, Int)]))]): Array[(Int, (String, Iterable[(String, Int)]))] = {
    val antitop_50 = anti_top50_rdd
      .sortBy(_._1).take(50)
       println("AntiTop-50 stemming")
    antitop_50.foreach(x => println(s"${x._1}: ${x._2._1}: ${x._2._2.map(y => y._1 + "-" + y._2).mkString(" ")}"))
    return antitop_50
  }

  //Refactoring text for stemming
  def RefText(refrdd: RDD[String]): RDD[(Int, (String, Iterable[(String, Int)]))] = {
    val rdd = refrdd.flatMap(line => line.split(" "))
      .map(word => (word, 1))
      .reduceByKey(_ + _)
    val rdd1 = rdd.map(x => (x._1, (x._2)))
    val rdd2 = rdd1.groupBy(x => stemming(x._1))
    val rdd3 = rdd2.map(s => ((s._2.map(y => y._2)).sum, s))
    return rdd3
  }

}