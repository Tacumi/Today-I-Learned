module AwesomeDateTest exposing (suite)

import AwesomeDate as Date exposing (Date)
import Expect
-- START:import.Fuzz
import Fuzz exposing (Fuzzer, int, intRange)
-- END:import.Fuzz
-- START:import.Random.Shrink
import Random
import Shrink
-- END:import.Random.Shrink
import Test exposing (..)


validLeapYears : List Int
validLeapYears =
    [ -400, -396, -392, -388, -384, -380, -376, -372, -368, -364, -360, -356, -352, -348, -344, -340, -336, -332, -328, -324, -320, -316, -312, -308, -304, -296, -292, -288, -284, -280, -276, -272, -268, -264, -260, -256, -252, -248, -244, -240, -236, -232, -228, -224, -220, -216, -212, -208, -204, -196, -192, -188, -184, -180, -176, -172, -168, -164, -160, -156, -152, -148, -144, -140, -136, -132, -128, -124, -120, -116, -112, -108, -104, -96, -92, -88, -84, -80, -76, -72, -68, -64, -60, -56, -52, -48, -44, -40, -36, -32, -28, -24, -20, -16, -12, -8, -4, 0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56, 60, 64, 68, 72, 76, 80, 84, 88, 92, 96, 104, 108, 112, 116, 120, 124, 128, 132, 136, 140, 144, 148, 152, 156, 160, 164, 168, 172, 176, 180, 184, 188, 192, 196, 204, 208, 212, 216, 220, 224, 228, 232, 236, 240, 244, 248, 252, 256, 260, 264, 268, 272, 276, 280, 284, 288, 292, 296, 304, 308, 312, 316, 320, 324, 328, 332, 336, 340, 344, 348, 352, 356, 360, 364, 368, 372, 376, 380, 384, 388, 392, 396, 400, 404, 408, 412, 416, 420, 424, 428, 432, 436, 440, 444, 448, 452, 456, 460, 464, 468, 472, 476, 480, 484, 488, 492, 496, 504, 508, 512, 516, 520, 524, 528, 532, 536, 540, 544, 548, 552, 556, 560, 564, 568, 572, 576, 580, 584, 588, 592, 596, 604, 608, 612, 616, 620, 624, 628, 632, 636, 640, 644, 648, 652, 656, 660, 664, 668, 672, 676, 680, 684, 688, 692, 696, 704, 708, 712, 716, 720, 724, 728, 732, 736, 740, 744, 748, 752, 756, 760, 764, 768, 772, 776, 780, 784, 788, 792, 796, 800, 804, 808, 812, 816, 820, 824, 828, 832, 836, 840, 844, 848, 852, 856, 860, 864, 868, 872, 876, 880, 884, 888, 892, 896, 904, 908, 912, 916, 920, 924, 928, 932, 936, 940, 944, 948, 952, 956, 960, 964, 968, 972, 976, 980, 984, 988, 992, 996, 1004, 1008, 1012, 1016, 1020, 1024, 1028, 1032, 1036, 1040, 1044, 1048, 1052, 1056, 1060, 1064, 1068, 1072, 1076, 1080, 1084, 1088, 1092, 1096, 1104, 1108, 1112, 1116, 1120, 1124, 1128, 1132, 1136, 1140, 1144, 1148, 1152, 1156, 1160, 1164, 1168, 1172, 1176, 1180, 1184, 1188, 1192, 1196, 1200, 1204, 1208, 1212, 1216, 1220, 1224, 1228, 1232, 1236, 1240, 1244, 1248, 1252, 1256, 1260, 1264, 1268, 1272, 1276, 1280, 1284, 1288, 1292, 1296, 1304, 1308, 1312, 1316, 1320, 1324, 1328, 1332, 1336, 1340, 1344, 1348, 1352, 1356, 1360, 1364, 1368, 1372, 1376, 1380, 1384, 1388, 1392, 1396, 1404, 1408, 1412, 1416, 1420, 1424, 1428, 1432, 1436, 1440, 1444, 1448, 1452, 1456, 1460, 1464, 1468, 1472, 1476, 1480, 1484, 1488, 1492, 1496, 1504, 1508, 1512, 1516, 1520, 1524, 1528, 1532, 1536, 1540, 1544, 1548, 1552, 1556, 1560, 1564, 1568, 1572, 1576, 1580, 1584, 1588, 1592, 1596, 1600, 1604, 1608, 1612, 1616, 1620, 1624, 1628, 1632, 1636, 1640, 1644, 1648, 1652, 1656, 1660, 1664, 1668, 1672, 1676, 1680, 1684, 1688, 1692, 1696, 1704, 1708, 1712, 1716, 1720, 1724, 1728, 1732, 1736, 1740, 1744, 1748, 1752, 1756, 1760, 1764, 1768, 1772, 1776, 1780, 1784, 1788, 1792, 1796, 1804, 1808, 1812, 1816, 1820, 1824, 1828, 1832, 1836, 1840, 1844, 1848, 1852, 1856, 1860, 1864, 1868, 1872, 1876, 1880, 1884, 1888, 1892, 1896, 1904, 1908, 1912, 1916, 1920, 1924, 1928, 1932, 1936, 1940, 1944, 1948, 1952, 1956, 1960, 1964, 1968, 1972, 1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016, 2020, 2024, 2028, 2032, 2036, 2040, 2044, 2048, 2052, 2056, 2060, 2064, 2068, 2072, 2076, 2080, 2084, 2088, 2092, 2096, 2104, 2108, 2112, 2116, 2120, 2124, 2128, 2132, 2136, 2140, 2144, 2148, 2152, 2156, 2160, 2164, 2168, 2172, 2176, 2180, 2184, 2188, 2192, 2196, 2204, 2208, 2212, 2216, 2220, 2224, 2228, 2232, 2236, 2240, 2244, 2248, 2252, 2256, 2260, 2264, 2268, 2272, 2276, 2280, 2284, 2288, 2292, 2296, 2304, 2308, 2312, 2316, 2320, 2324, 2328, 2332, 2336, 2340, 2344, 2348, 2352, 2356, 2360, 2364, 2368, 2372, 2376, 2380, 2384, 2388, 2392, 2396, 2400, 2404, 2408, 2412, 2416, 2420, 2424, 2428, 2432, 2436, 2440, 2444, 2448, 2452, 2456, 2460, 2464, 2468, 2472, 2476, 2480, 2484, 2488, 2492, 2496, 2504, 2508, 2512, 2516, 2520, 2524, 2528, 2532, 2536, 2540, 2544, 2548, 2552, 2556, 2560, 2564, 2568, 2572, 2576, 2580, 2584, 2588, 2592, 2596, 2604, 2608, 2612, 2616, 2620, 2624, 2628, 2632, 2636, 2640, 2644, 2648, 2652, 2656, 2660, 2664, 2668, 2672, 2676, 2680, 2684, 2688, 2692, 2696, 2704, 2708, 2712, 2716, 2720, 2724, 2728, 2732, 2736, 2740, 2744, 2748, 2752, 2756, 2760, 2764, 2768, 2772, 2776, 2780, 2784, 2788, 2792, 2796, 2800, 2804, 2808, 2812, 2816, 2820, 2824, 2828, 2832, 2836, 2840, 2844, 2848, 2852, 2856, 2860, 2864, 2868, 2872, 2876, 2880, 2884, 2888, 2892, 2896, 2904, 2908, 2912, 2916, 2920, 2924, 2928, 2932, 2936, 2940, 2944, 2948, 2952, 2956, 2960, 2964, 2968, 2972, 2976, 2980, 2984, 2988, 2992, 2996 ]


exampleDate : Date
exampleDate =
    Date.create 2012 6 2


leapDate : Date
leapDate =
    Date.create 2012 2 29


-- START:dateFuzzer
dateFuzzer : Fuzzer ( Int, Int, Int )
dateFuzzer =
    let
        randomYear = Random.int Random.minInt Random.maxInt
        randomMonth = Random.int 1 12
        generator =
            Random.pair randomYear randomMonth
                |> Random.andThen
                    (\( year, month ) ->
                        Random.int 1 (Date.daysInMonth year month)
                            |> Random.map (\day -> ( year, month, day ))
                    )
        shrinker dateTuple =
            Shrink.tuple3 ( Shrink.int, Shrink.int, Shrink.int ) dateTuple
    in
    Fuzz.custom generator shrinker
-- END:dateFuzzer


expectDate : Int -> Int -> Int -> Date -> Expect.Expectation
expectDate year month day actualDate =
    let
        expectedDate =
            Date.create year month day
    in
    if actualDate == expectedDate then
        Expect.pass

    else
        Expect.fail <|
            Date.toDateString actualDate
                ++ "\n???\n??? expectDate\n???\n"
                ++ Date.toDateString expectedDate


testDateParts : Test
testDateParts =
    describe "date part getters"
        [ test "retrieves the year from a date" <|
            \_ ->
                Date.year exampleDate
                    |> Expect.equal 2012
        , test "retrieves the month from a date" <|
            \_ ->
                Date.month exampleDate
                    |> Expect.equal 6
        , test "retrieves the day from a date" <|
            \_ ->
                Date.day exampleDate
                    |> Expect.equal 2
        ]


testIsLeapYear : Test
testIsLeapYear =
    -- START:testIsLeapYear
    describe "isLeapYear"
        [ fuzz (intRange -400 3000) "determines leap years correctly" <|
            \year ->
                if List.member year validLeapYears then
                    Date.isLeapYear year
                        |> Expect.true "Expected leap year"

                else
                    Date.isLeapYear year
                        |> Expect.false "Did not expect leap year"
        ]
    -- END:testIsLeapYear


testAddYears : Test
testAddYears =
    describe "addYears"
        [ test "changes a date's year" <|
            \_ ->
                Date.addYears 2 exampleDate
                    |> expectDate 2014 6 2
        , test "prevents leap days on non-leap years" <|
            \_ ->
                Date.addYears 1 leapDate
                    |> expectDate 2013 2 28
        , fuzz2 int dateFuzzer "changes the year by the amount given" <|
            \years ( year, month, day ) ->
                let
                    date =
                        Date.create year month day

                    newDate =
                        Date.addYears years date
                in
                (Date.year newDate - Date.year date)
                    |> Expect.equal years
        ]


-- START:testToDateString
testToDateString : Test
testToDateString =
    describe "toDateString"
        [ fuzz dateFuzzer "creates a valid date string" <|
            \( year, month, day ) ->
                Date.create year month day
                    |> Date.toDateString
                    |> Expect.equal
                        (String.fromInt month
                            ++ "/"
                            ++ String.fromInt day
                            ++ "/"
                            ++ String.fromInt year
                        )
        ]
-- END:testToDateString


suite : Test
suite =
    describe "AwesomeDate"
        [ testDateParts
        , testIsLeapYear
        , testAddYears
        , testToDateString
        ]
