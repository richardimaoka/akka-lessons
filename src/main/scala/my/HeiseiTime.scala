package my

import com.datastax.driver.core.LocalDate

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
    System.out.printf(" t: %s%n", dtf.parse("H.10.11.12", JapaneseDate::from))

  }
}


## Pre-requisite:

Get the following ssh keys so that you can log into data servers.
Ask other Platform engineers or ask taki to get the keys.

* `paidy-v2-development.pem`
* `paidy-v2-production.pem`

Create a file `~/.ssh/config` with [this content](https://github.com/ExchangeCorporation/paidy-core/wiki/How-to-reset-projection/_edit#ssh-config-content).

## Confirm the issue is due to missing events NOT picked up by a projection

* Projection's most-recently processed event's timestamp should be close to the current time (i.e. no lag in projection)
* Expected events are all in Cassandra (if events are not even in Cassandra, that's a different issue)

### Login to the data server.

* PROD: `ssh dataserver-0.production.paidy.io.internal`
* UAT :

### Login to Cassandra shell

`docker exec -it cassandra-1 cqlsh`

### Check the events sent to Cassandra

`select * from akka.messages where persistence_id=‘pay_WXbdqE4AAEkAe2fn’ and partition_nr=0;`

You should verify here if all the expected events are in Cassandra.
And from the next step, you will see if one or multiple events are missing in Projections.

### Check the projection's most recent event's timestamp

In the same Cassandra shell, do the following:

`cqlsh> select * from akka.offsets;`

```
processor_id                    | tag | time_uuid_offset
--------------------------------+-----+--------------------------------------
...
...
CaptureToMerchantCredit         |     | e37fd090-75be-11e7-b85c-87a2557057a1
PaidyPaymentView                |     | e4545ea0-75be-11e7-b85c-87a2557057a1   // <------ for payment projection
CombiniPaymentToWebhooks        |     | db2c8220-75bf-11e7-b3c4-6fbf4ce87adf
...
...
```

Pick the UUID of the projection you want to check, and paste the UUID in this web page.

https://www.famkruithof.net/uuid/uuidgen?typeReq=-1

You see the UUID `e4545ea0-75be-11e7-b85c-87a2557057a1` is translated to `Monday, July 31, 2017 4:07:19 PM JST`.
This should be close enough to the current time. Otherwise, there is a lag in projection.


### Proceed further or not??

At this point, you should be able to tell whether the projection missed some events as mentioned above.
Do the below if there are indeed missing events.

## Reset and restart the projection

* `curl --insecure https://production.paidy.io/core-api-internal/projections/payments`

Replace `projections/payments` to the projection you want to check. This tells you the current projection status.
(must be status = running)


### Make announcement in Slack

Use `@here`

<img width="587" alt="screen shot 2017-07-31 at 16 44 26" src="https://user-images.githubusercontent.com/7414320/28768048-2f5e09c6-7611-11e7-9ead-dfb04ff641f6.png">

### Stop projection

The HTTP `DELETE` method will stop the projection.

* `curl --insecure -XDELETE https://production.paidy.io/core-api-internal/projections/payments`

Check the status and make sure it is "stopped"

* `curl --insecure https://production.paidy.io/core-api-internal/projections/payments`
  * (`curl --insecure https://production.paidy.io/core-api-internal/projections/payments | jq` for pretty-print)

### Reset offset

Offset is the time offset from the base time (1970 ??), represented as UUID. To get the time offset (UUID) to insert to
the projection, you need to execute the following scala code.

* Launch sbt under paidy-core
* `project payments`
* because you need jars available
* `console`
* do the following:

```
scala> import com.datastax.driver.core.utils._
scala> import org.joda.time.DateTime
scala> UUIDs.startOf(new DateTime(2017, 7, 25, 05, 0, 0).getMillis)
```

In the Cassandra shell, you can reset the offset as follows,

`update akka.offsets set time_uuid_offset=ac90a000-70aa-11e7-8080-808080808080 where processor_id='PaidyPaymentView' and tag='';`

* Replace 'PaidyPaymentView' with the projection name you want to reset.
* Also replace `ac90a000-70aa-11e7-8080-808080808080` with the UUID you got from the previous step.

In the Cassandra shell, do this again to make sure the offset is reset:

`select * from akka.offsets;`

### Restart projection

The HTTP `POST` method will restart the projection.

`curl --insecure -XPOST https://production.paidy.io/core-api-internal/projections/payments`


### Appending: ssh config content

```
Host exco-nat exco-tunnel paidy-adm01 paidy-adm02 paidy-bch01 paidy-bch02 paidy-dba01 paidy-dbm01 paidy-dbm02  rabbit1a paidy-ec01     paidy-jenkins01 jenkins paidy-build01 paidy-load01 paidy-mon01 mon paidy-nat01 paidy-nat02 paidy-proxy01 paidy-proxy02 apipaidyuat paidy-wap02 paidy-we    b01 paidy-web02
User exco
IdentityFile ~/.ssh/exco.pem
Host dev
HostName paidy-dev01
User exco
IdentityFile ~/.ssh/exco.pem
LocalForward localhost:27023 localhost:27017
Host uat
HostName paidy-uat01
User exco
IdentityFile ~/.ssh/exco.pem
LocalForward localhost:27025 localhost:27017
Host prd
HostName paidy-wap01
User exco
IdentityFile ~/.ssh/exco.pem
LocalForward localhost:27022 localhost:27017
Host uat-coreos* test-coreos*
User core
StrictHostKeyChecking no
IdentityFile ~/.ssh/paidy-v2-development.pem
Host production-coreos*
User core
StrictHostKeyChecking no
IdentityFile ~/.ssh/paidy-v2-production.pem
Host mongo-v2
Hostname 172.31.27.70
User ubuntu
StrictHostKeyChecking no
IdentityFile ~/.ssh/paidy-v2-development.pem
Host lb.*.paidy.io.internal redis.*.paidy.io.internal elastic.*.paidy.io.internal cassandra.*.paidy.io.internal akka*.*.paidy.io.internal akka-left.*.paidy.io.internal akka-right.*.paidy.io.internal dataserver-*.*.paidy.io.internal
User ubuntu
StrictHostKeyChecking no
IdentityFile ~/.ssh/paidy-v2-development.pem
Host *.paidy.io.internal
User ubuntu
StrictHostKeyChecking no
IdentityFile ~/.ssh/paidy-v2-development.pem

```
