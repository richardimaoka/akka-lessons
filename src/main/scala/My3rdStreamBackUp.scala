//object My3rdStreamBackUp {
//  def main(args: Array[String]): Unit = {
//
//    Sink.head
//
//    val uri = Uri("http://localhost?a^=b", mode=Uri.ParsingMode.Relaxed)
//
//    //prints Some(b)
//    println(uri.query(mode=Uri.ParsingMode.Relaxed).get("a^"))
//
//    //IllegalUriException: Illegal query: Invalid input '^',
//    // expected '+', '=', query-char, 'EOI', '&' or pct-encoded
//    // (line 1, column 2): a^=b
//    //println(uri.query(mode=Uri.ParsingMode.Strict).get("a^"))
//
//
//    val uri2 = Uri("http://localhost?a+b=c", mode=Uri.ParsingMode.Relaxed)
//
//    //prints Some(b)
//    println(uri2.query(mode=Uri.ParsingMode.Relaxed).get("a^"))
//
//    val uri3 = Uri("localhost?uri=http://ahost/aPath?aParam%3DaValue%23aFragment#aFragment")
//
//    println(uri3)
//
//
//    val uri4 = Uri("http://localhost?a=%2520&b=s", mode=Uri.ParsingMode.Relaxed)
//
//    println(uri4.queryString())
//
//    println( Query("a=b" → "c") )
//
//    //Uri("foö")
//
//    //println(Uri("http://loc#$^alhost?a=^b", Uri.ParsingMode.Strict))
//
//    println(Uri("http://ftp.is.co.za/rfc/rfc1808.txt?a#=b"))
//    println(Uri.from(scheme = "ftp", host = "ftp.is.co.za", path = "/rfc/rfc1808.txt", queryString = Some("a=b=%b")))
//
//    println( NamedHost("""/hällö\""") )
//    println( Uri("http://ssss%2Fh%C3%A4ll%C3%B6%5C?a=b").authority )
//
//  }
//}
