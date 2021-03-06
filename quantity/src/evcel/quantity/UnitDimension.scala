package evcel.quantity

import evcel.utils.Enumerate

case class UnitDimension(prime: Int, names: List[String])

object UnitDimension extends Enumerate[UnitDimension](classOf[UnitDimension], _.names){
  private val primes = Primes.primes()
  lazy val primesUsed: Seq[Int] = values.map(_.prime)

  def apply(names: String*) = {
    new UnitDimension(primes.next(), names.toList)
  }

  val SCALAR = new UnitDimension(1, "SCALAR" :: Nil)
  val Mass = UnitDimension("Mass")
  val Volume = UnitDimension("Volume")
  val Time = UnitDimension("Time")
  val Length = UnitDimension("Length")

  // An oil barrel (abbreviated as bbl) is a unit of volume whose definition has not been universally standardized.
  // from http://en.wikipedia.org/wiki/Oil_barrel#Oil_barrel
  val OilVolume = UnitDimension("OilVolume")

  // One British therm (symbol thm) is a non-SI unit of heat energy equal to 100,000 British thermal units (BTU).
  // It is approximately the energy equivalent of burning 100 cubic feet (often referred to as 1 CCF) of natural gas.
  // from http://en.wikipedia.org/wiki/Therm
  val GasVolume = UnitDimension("GasVolume")

  // currencies
  val AED = UnitDimension("AED") // United Arab Emirates Dirham
  val AFN = UnitDimension("AFN") // Afghanistan Afghani
  val ALL = UnitDimension("ALL") // Albania Lek
  val AMD = UnitDimension("AMD") // Armenia Dram
  val ANG = UnitDimension("ANG") // Netherlands Antilles Guilder
  val AOA = UnitDimension("AOA") // Angola Kwanza
  val ARS = UnitDimension("ARS") // Argentina Peso
  val AUD = UnitDimension("AUD") // Australia Dollar
  val AWG = UnitDimension("AWG") // Aruba Guilder
  val AZN = UnitDimension("AZN") // Azerbaijan New Manat
  val BAM = UnitDimension("BAM") // Bosnia and Herzegovina Convertible Marka
  val BBD = UnitDimension("BBD") // Barbados Dollar
  val BDT = UnitDimension("BDT") // Bangladesh Taka
  val BGN = UnitDimension("BGN") // Bulgaria Lev
  val BHD = UnitDimension("BHD") // Bahrain Dinar
  val BIF = UnitDimension("BIF") // Burundi Franc
  val BMD = UnitDimension("BMD") // Bermuda Dollar
  val BND = UnitDimension("BND") // Brunei Darussalam Dollar
  val BOB = UnitDimension("BOB") // Bolivia Boliviano
  val BRL = UnitDimension("BRL") // Brazil Real
  val BSD = UnitDimension("BSD") // Bahamas Dollar
  val BTN = UnitDimension("BTN") // Bhutan Ngultrum
  val BWP = UnitDimension("BWP") // Botswana Pula
  val BYR = UnitDimension("BYR") // Belarus Ruble
  val BZD = UnitDimension("BZD") // Belize Dollar
  val CAD = UnitDimension("CAD") // Canada Dollar
  val CDF = UnitDimension("CDF") // Congo/Kinshasa Franc
  val CHF = UnitDimension("CHF") // Switzerland Franc
  val CLP = UnitDimension("CLP") // Chile Peso
  val CNY = UnitDimension("CNY") // China Yuan Renminbi
  val COP = UnitDimension("COP") // Colombia Peso
  val CRC = UnitDimension("CRC") // Costa Rica Colon
  val CUC = UnitDimension("CUC") // Cuba Convertible Peso
  val CUP = UnitDimension("CUP") // Cuba Peso
  val CVE = UnitDimension("CVE") // Cape Verde Escudo
  val CZK = UnitDimension("CZK") // Czech Republic Koruna
  val DJF = UnitDimension("DJF") // Djibouti Franc
  val DKK = UnitDimension("DKK") // Denmark Krone
  val DOP = UnitDimension("DOP") // Dominican Republic Peso
  val DZD = UnitDimension("DZD") // Algeria Dinar
  val EGP = UnitDimension("EGP") // Egypt Pound
  val ERN = UnitDimension("ERN") // Eritrea Nakfa
  val ETB = UnitDimension("ETB") // Ethiopia Birr
  val EUR = UnitDimension("EUR") // Euro Member Countries
  val FJD = UnitDimension("FJD") // Fiji Dollar
  val FKP = UnitDimension("FKP") // Falkland Islands (Malvinas) Pound
  val GBP = UnitDimension("GBP") // United Kingdom Pound
  val GEL = UnitDimension("GEL") // Georgia Lari
  val GGP = UnitDimension("GGP") // Guernsey Pound
  val GHS = UnitDimension("GHS") // Ghana Cedi
  val GIP = UnitDimension("GIP") // Gibraltar Pound
  val GMD = UnitDimension("GMD") // Gambia Dalasi
  val GNF = UnitDimension("GNF") // Guinea Franc
  val GTQ = UnitDimension("GTQ") // Guatemala Quetzal
  val GYD = UnitDimension("GYD") // Guyana Dollar
  val HKD = UnitDimension("HKD") // Hong Kong Dollar
  val HNL = UnitDimension("HNL") // Honduras Lempira
  val HRK = UnitDimension("HRK") // Croatia Kuna
  val HTG = UnitDimension("HTG") // Haiti Gourde
  val HUF = UnitDimension("HUF") // Hungary Forint
  val IDR = UnitDimension("IDR") // Indonesia Rupiah
  val ILS = UnitDimension("ILS") // Israel Shekel
  val IMP = UnitDimension("IMP") // Isle of Man Pound
  val INR = UnitDimension("INR") // India Rupee
  val IQD = UnitDimension("IQD") // Iraq Dinar
  val IRR = UnitDimension("IRR") // Iran Rial
  val ISK = UnitDimension("ISK") // Iceland Krona
  val JEP = UnitDimension("JEP") // Jersey Pound
  val JMD = UnitDimension("JMD") // Jamaica Dollar
  val JOD = UnitDimension("JOD") // Jordan Dinar
  val JPY = UnitDimension("JPY") // Japan Yen
  val KES = UnitDimension("KES") // Kenya Shilling
  val KGS = UnitDimension("KGS") // Kyrgyzstan Som
  val KHR = UnitDimension("KHR") // Cambodia Riel
  val KMF = UnitDimension("KMF") // Comoros Franc
  val KPW = UnitDimension("KPW") // Korea (North) Won
  val KRW = UnitDimension("KRW") // Korea (South) Won
  val KWD = UnitDimension("KWD") // Kuwait Dinar
  val KYD = UnitDimension("KYD") // Cayman Islands Dollar
  val KZT = UnitDimension("KZT") // Kazakhstan Tenge
  val LAK = UnitDimension("LAK") // Laos Kip
  val LBP = UnitDimension("LBP") // Lebanon Pound
  val LKR = UnitDimension("LKR") // Sri Lanka Rupee
  val LRD = UnitDimension("LRD") // Liberia Dollar
  val LSL = UnitDimension("LSL") // Lesotho Loti
  val LTL = UnitDimension("LTL") // Lithuania Litas
  val LYD = UnitDimension("LYD") // Libya Dinar
  val MAD = UnitDimension("MAD") // Morocco Dirham
  val MDL = UnitDimension("MDL") // Moldova Leu
  val MGA = UnitDimension("MGA") // Madagascar Ariary
  val MKD = UnitDimension("MKD") // Macedonia Denar
  val MMK = UnitDimension("MMK") // Myanmar (Burma) Kyat
  val MNT = UnitDimension("MNT") // Mongolia Tughrik
  val MOP = UnitDimension("MOP") // Macau Pataca
  val MRO = UnitDimension("MRO") // Mauritania Ouguiya
  val MUR = UnitDimension("MUR") // Mauritius Rupee
  val MVR = UnitDimension("MVR") // Maldives (Maldive Islands) Rufiyaa
  val MWK = UnitDimension("MWK") // Malawi Kwacha
  val MXN = UnitDimension("MXN") // Mexico Peso
  val MYR = UnitDimension("MYR") // Malaysia Ringgit
  val MZN = UnitDimension("MZN") // Mozambique Metical
  val NAD = UnitDimension("NAD") // Namibia Dollar
  val NGN = UnitDimension("NGN") // Nigeria Naira
  val NIO = UnitDimension("NIO") // Nicaragua Cordoba
  val NOK = UnitDimension("NOK") // Norway Krone
  val NPR = UnitDimension("NPR") // Nepal Rupee
  val NZD = UnitDimension("NZD") // New Zealand Dollar
  val OMR = UnitDimension("OMR") // Oman Rial
  val PAB = UnitDimension("PAB") // Panama Balboa
  val PEN = UnitDimension("PEN") // Peru Nuevo Sol
  val PGK = UnitDimension("PGK") // Papua New Guinea Kina
  val PHP = UnitDimension("PHP") // Philippines Peso
  val PKR = UnitDimension("PKR") // Pakistan Rupee
  val PLN = UnitDimension("PLN") // Poland Zloty
  val PYG = UnitDimension("PYG") // Paraguay Guarani
  val QAR = UnitDimension("QAR") // Qatar Riyal
  val RON = UnitDimension("RON") // Romania New Leu
  val RSD = UnitDimension("RSD") // Serbia Dinar
  val RUB = UnitDimension("RUB") // Russia Ruble
  val RWF = UnitDimension("RWF") // Rwanda Franc
  val SAR = UnitDimension("SAR") // Saudi Arabia Riyal
  val SBD = UnitDimension("SBD") // Solomon Islands Dollar
  val SCR = UnitDimension("SCR") // Seychelles Rupee
  val SDG = UnitDimension("SDG") // Sudan Pound
  val SEK = UnitDimension("SEK") // Sweden Krona
  val SGD = UnitDimension("SGD") // Singapore Dollar
  val SHP = UnitDimension("SHP") // Saint Helena Pound
  val SLL = UnitDimension("SLL") // Sierra Leone Leone
  val SOS = UnitDimension("SOS") // Somalia Shilling
  val SPL = UnitDimension("SPL") // Seborga Luigino
  val SRD = UnitDimension("SRD") // Suriname Dollar
  val STD = UnitDimension("STD") // São Tomé and Príncipe Dobra
  val SVC = UnitDimension("SVC") // El Salvador Colon
  val SYP = UnitDimension("SYP") // Syria Pound
  val SZL = UnitDimension("SZL") // Swaziland Lilangeni
  val THB = UnitDimension("THB") // Thailand Baht
  val TJS = UnitDimension("TJS") // Tajikistan Somoni
  val TMT = UnitDimension("TMT") // Turkmenistan Manat
  val TND = UnitDimension("TND") // Tunisia Dinar
  val TOP = UnitDimension("TOP") // Tonga Pa'anga
  val TRY = UnitDimension("TRY") // Turkey Lira
  val TTD = UnitDimension("TTD") // Trinidad and Tobago Dollar
  val TVD = UnitDimension("TVD") // Tuvalu Dollar
  val TWD = UnitDimension("TWD") // Taiwan New Dollar
  val TZS = UnitDimension("TZS") // Tanzania Shilling
  val UAH = UnitDimension("UAH") // Ukraine Hryvnia
  val UGX = UnitDimension("UGX") // Uganda Shilling
  val USD = UnitDimension("USD") // United States Dollar
  val UYU = UnitDimension("UYU") // Uruguay Peso
  val UZS = UnitDimension("UZS") // Uzbekistan Som
  val VEF = UnitDimension("VEF") // Venezuela Bolivar
  val VND = UnitDimension("VND") // Viet Nam Dong
  val VUV = UnitDimension("VUV") // Vanuatu Vatu
  val WST = UnitDimension("WST") // Samoa Tala
  val XAF = UnitDimension("XAF") // Communauté Financière Africaine (BEAC) CFA Franc BEAC
  val XCD = UnitDimension("XCD") // East Caribbean Dollar
  val XDR = UnitDimension("XDR") // International Monetary Fund (IMF) Special Drawing Rights
  val XOF = UnitDimension("XOF") // Communauté Financière Africaine (BCEAO) Franc
  val XPF = UnitDimension("XPF") // Comptoirs Français du Pacifique (CFP) Franc
  val YER = UnitDimension("YER") // Yemen Rial
  val ZAR = UnitDimension("ZAR") // South Africa Rand
  val ZMW = UnitDimension("ZMW") // Zambia Kwacha
  val ZWD = UnitDimension("ZWD") // Zimbabwe Dollar

  val currencyPrimes =
    Set(AED,AFN,ALL,AMD,ANG,AOA,ARS,AUD,AWG,AZN,BAM,BBD,BDT,BGN,BHD,BIF,BMD,BND,BOB,BRL,BSD,BTN,BWP,BYR,
    BZD,CAD,CDF,CHF,CLP,CNY,COP,CRC,CUC,CUP,CVE,CZK,DJF,DKK,DOP,DZD,EGP,ERN,ETB,EUR,FJD,FKP,GBP,GEL,GGP,GHS,GIP,GMD,
    GNF,GTQ,GYD,HKD,HNL,HRK,HTG,HUF,IDR,ILS,IMP,INR,IQD,IRR,ISK,JEP,JMD,JOD,JPY,KES,KGS,KHR,KMF,KPW,KRW,KWD,KYD,KZT,
    LAK,LBP,LKR,LRD,LSL,LTL,LYD,MAD,MDL,MGA,MKD,MMK,MNT,MOP,MRO,MUR,MVR,MWK,MXN,MYR,MZN,NAD,NGN,NIO,NOK,NPR,NZD,OMR,
    PAB,PEN,PGK,PHP,PKR,PLN,PYG,QAR,RON,RSD,RUB,RWF,SAR,SBD,SCR,SDG,SEK,SGD,SHP,SLL,SOS,SPL,SRD,STD,SVC,SYP,SZL,THB,
    TJS,TMT,TND,TOP,TRY,TTD,TVD,TWD,TZS,UAH,UGX,USD,UYU,UZS,VEF,VND,VUV,WST,XAF,XCD,XDR,XOF,XPF,YER,ZAR,ZMW,ZWD).map{
      _.prime
    }
}
