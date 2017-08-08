package my

import java.time.chrono.JapaneseEra
import java.time.format.DateTimeFormatterBuilder
import java.time.temporal.ChronoField

object HeiseiTime {
  def main(args: Array[String]): Unit = {
    import java.time.chrono.{JapaneseChronology, JapaneseDate}
    import java.time.format.DateTimeFormatter

    import java.time.LocalDate

    val imperialShortFormat = DateTimeFormatter.ofPattern("GGGGGyyMMdd").withChronology(JapaneseChronology.INSTANCE)
    //imperialShortFormat: java.time.format.DateTimeFormatter = Text(Era,NARROW)ReducedValue(YearOfEra,2,2,2000-01-01)Value(MonthOfYear,2)Value(DayOfMonth,2)

    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
    //formatter: java.time.format.DateTimeFormatter = Value(YearOfEra,4,19,EXCEEDS_PAD)'-'Value(MonthOfYear,2)'-'Value(DayOfMonth,2)' 'Value(HourOfDay,2)':'Value(MinuteOfHour,2)':'Value(SecondOfMinute,2)

    val date = LocalDate.parse("1994-09-27 09:00:00", formatter);
    //date: java.time.LocalDate = 1994-09-27

    val jpDate = JapaneseDate.of(date.getYear, date.getMonthValue, date.getDayOfMonth) // java.time.chrono.JapaneseDate = Japanese Heisei 6-09-27
    //jpDate: java.time.chrono.JapaneseDate = Japanese Heisei 6-09-27

    val jpDateStr = imperialShortFormat.format(jpDate) // String = H060927
    //jpDateStr: String = H060927

    val temporal = imperialShortFormat.parse(jpDateStr)
    //temporal: java.time.temporal.TemporalAccessor = {},Japanese resolved to Japanese Heisei 106-09-27

    val jpDate2 = LocalDate.from(temporal) //java.time.LocalDate = 2094-09-27
    //jpDate: java.time.LocalDate = 2094-09-27
    println(jpDate2)

    val basedate: JapaneseDate = JapaneseChronology.INSTANCE.date(JapaneseEra.HEISEI, 1989, 1, 1);
    val dtf: DateTimeFormatter = new DateTimeFormatterBuilder()
      .appendPattern("GGGGG.")
      .appendValueReduced(ChronoField.YEAR_OF_ERA, 2, 2, basedate)
      .appendPattern(".MM.dd")
      .toFormatter().withChronology(JapaneseChronology.INSTANCE)
    //System.out.printf(" t: %s%n", dtf.parse("H.10.11.12", JapaneseDate.from))

  }
}
