\name{immun}
\alias{immun}
\docType{data}
\title{Body weight data}
\description{Body weights: weeks 1, 8, 15, 22, 29 in 0, 2, 10 ,20, 100 mg/kg dose}
\usage{data("immun")}
\format{
  A data frame with 1000 observations on the following 24 variables.
  \describe{
    \item{\code{chemical_name}}{a factor with levels \code{Chloramine}}
    \item{\code{cas_no}}{a factor with levels \code{10599-90-3}}
    \item{\code{study_no}}{a factor with levels \code{IMM20007}}
    \item{\code{study_route}}{a factor with levels \code{Dosed-Water}}
    \item{\code{ANIMAL_NO}}{a numeric vector}
    \item{\code{CONTROL_CODE}}{a factor with levels \code{} \code{P} \code{V}}
    \item{\code{DOSE}}{a numeric vector}
    \item{\code{DOSE_GROUP}}{a factor with levels \code{D1} \code{D2} \code{D3} \code{D4} \code{D5} \code{PC1} \code{PC2} \code{PC3} \code{VH}}
    \item{\code{DOSE_SCHEDULE}}{a factor with levels \code{adl} \code{Once} \code{qd (last 24 hrs)} \code{qd (last 4 days)}}
    \item{\code{DOSE_UNITS}}{a factor with levels \code{mg/kg} \code{ml (1:10)} \code{ppm}}
    \item{\code{FOOTNOTEMARKER}}{a logical vector}
    \item{\code{MEASUREMENTCODE}}{a factor with levels \code{BODY_WT_G}}
    \item{\code{MEASUREMENTVALUE}}{a numeric vector}
    \item{\code{MEASUREMENT_TIME}}{a factor with levels \code{Day 1} \code{Day 15} \code{Day 22} \code{Day 29} \code{Day 8}}
    \item{\code{OBS_TIME}}{a numeric vector}
    \item{\code{POSITIVE_CONTROL}}{a factor with levels \code{} \code{Anti Asialo GM1 Antibody} \code{Cyclophosphamide}}
    \item{\code{ROUTE}}{a factor with levels \code{IP/IJ} \code{IV} \code{WATER}}
    \item{\code{TEST_GROUP}}{a numeric vector}
    \item{\code{VEHICLE}}{a factor with levels \code{Phosphate Buffered Saline} \code{Saline} \code{Tap Water}}
    \item{\code{X}}{a logical vector}
    \item{\code{dose}}{a factor with levels \code{0} \code{2} \code{10} \code{20} \code{100}}
    \item{\code{weight}}{a numeric vector}
    \item{\code{Anino}}{a factor with levels \code{1} \code{2} \code{3} \code{4} \code{5} \code{6} \code{7} \code{8} \code{9} \code{10} \code{11} \code{12} \code{13} \code{14} \code{15} \code{16} \code{17} \code{18} \code{19} \code{20} \code{21} \code{22} \code{23} \code{24} \code{25} \code{26} \code{27} \code{28} \code{29} \code{30} \code{31} \code{32} \code{33} \code{34} \code{35} \code{36} \code{37} \code{38} \code{39} \code{40} \code{57} \code{58} \code{59} \code{60} \code{61} \code{62} \code{63} \code{64} \code{65} \code{66} \code{67} \code{68} \code{69} \code{70} \code{71} \code{72} \code{73} \code{74} \code{75} \code{76} \code{77} \code{78} \code{79} \code{80} \code{81} \code{82} \code{83} \code{84} \code{85} \code{86} \code{87} \code{88} \code{89} \code{90} \code{91} \code{92} \code{93} \code{94} \code{95} \code{96} \code{121} \code{122} \code{123} \code{124} \code{125} \code{126} \code{127} \code{128} \code{129} \code{130} \code{131} \code{132} \code{133} \code{134} \code{135} \code{136} \code{137} \code{138} \code{139} \code{140} \code{141} \code{142} \code{143} \code{144} \code{145} \code{146} \code{147} \code{148} \code{149} \code{150} \code{151} \code{152} \code{153} \code{154} \code{155} \code{156} \code{157} \code{158} \code{159} \code{160} \code{177} \code{178} \code{179} \code{180} \code{181} \code{182} \code{183} \code{184} \code{185} \code{186} \code{187} \code{188} \code{189} \code{190} \code{191} \code{192} \code{193} \code{194} \code{195} \code{196} \code{197} \code{198} \code{199} \code{200} \code{201} \code{202} \code{203} \code{204} \code{205} \code{206} \code{207} \code{208} \code{209} \code{210} \code{211} \code{212} \code{213} \code{214} \code{215} \code{216} \code{233} \code{234} \code{235} \code{236} \code{237} \code{238} \code{239} \code{240} \code{241} \code{242} \code{243} \code{244} \code{245} \code{246} \code{247} \code{248} \code{249} \code{250} \code{251} \code{252} \code{253} \code{254} \code{255} \code{256} \code{257} \code{258} \code{259} \code{260} \code{261} \code{262} \code{263} \code{264} \code{265} \code{266} \code{267} \code{268} \code{269} \code{270} \code{271} \code{272}}
    \item{\code{time}}{a numeric vector}
  }
}
\source{National Toxicology Program. 28 days Immunotoxicity bioassay on mice treated with
Chloramine (2000)}
\examples{
data(immun)
}
\keyword{datasets}
