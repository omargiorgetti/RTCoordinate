#' Funzione che restituisce le cordinate di un indirizzo utilizzando il servizio mappe della regione toscana
#' @param indirizzo
#' @param civico
#' @param comune
#' @param siglaprovincia
#' @param url
#' @examples
#' indirizzo <- 'via di novoli'
#' civico <- '26'
#' comune <- 'firenze'
#' provincia <- 'Firenze'
#' siglaprovincia <- 'FI'
#' URLRT <- 'http://mappe.regione.toscana.it/legacy-musume/services/Musume?method=richiesta&algoritmo=1&'
#' coordRT <- getCoordRT(indirizzo,civico,comune,siglaprovincia,URLRT)
#'
#' @return vettore con longitudie e latitudine e una stringa che contiene:
#' - la url utilizzata per ottenere le coordinati se vengono trovate
#' - NOOKResponse se la chiamata della url ha dato errore
#' - NOCOORD se la riposta ha dato esito positivo ma l'indirizzo non è interpretabile
#' @export
getCoordRT <- function(indirizzo,civico,comune,provincia,url){

  posturl <- '&fonte=1'
  indirizzo <- paste0(indirizzo,' ',civico)
  getstr <- paste0(url,'provincia=',provincia,'&comune=',gsub(' ','%20',comune),'&indirizzo=',gsub(' ','%20',indirizzo),posturl)
  print(getstr)
  getstrg <- httr::GET(getstr)
  print(getstrg)
  status <- getstrg$status_code
  print(paste0('status: ',status))
  if (status!=400){
    print('OK')
    ris <- xml2::as_xml_document(getstrg)
    nodes <- xml2::xml_find_all(ris,'.//multiRef')
    sezioneid2 <- nodes[xml2::xml_attr(nodes,'id')=='id2']
    lat_rt <- as.double(xml2::xml_text(xml2::xml_find_first(sezioneid2,'.//latitudine')))
    lon_rt <- as.double(xml2::xml_text(xml2::xml_find_first(sezioneid2,'.//longitudine')))
    if (is.na(lat_rt) || is.na(lon_rt)){
      print('NOCOORD')
      getCoordRT <- c(0,0,'NOCOORD')
    }
    else{
      getCoordRT <- c(lon_rt,lat_rt,getstr)
    }

  }else
  {
    print('NOOKResponse')
    getCoordRT <- c(0,0,'NOOKResponse')}

}

#' Funzione che restituisce le cordinate di un indirizzo utilizzando il servizio openruoteservice
#' @param indirizzo
#' @param civico
#' @param comune
#' @param provincia
#' @param TOKEN
#' @examples
#' indirizzo <- 'via di novoli'
#' civico <- '26'
#' comune <- 'firenze'
#' provincia <- 'Firenze'
#' siglaprovincia <- 'FI'
#' token <- '5b3ce359785111000we624823fe6724fd45c3681e64432745a3bec'
#' coordORS <- getCoordORS(indirizzo,civico,comune,provincia,token)
#'
#' @return vettore con longitudie e latitudine
#' NA se l'indirizzo non viene decodificato
#' @export
getCoordORS <- function(indirizzo,civico,comune,provincia,token){
  query <- paste0(indirizzo,' ',civico,', ',comune,', ',provincia)
  ris_ors <- openrouteservice::ors_geocode(query = query,size=1,source='osm',boundary.country='IT',api_key = token)
  if (length(ris_ors$features)>0){
    point <- ris_ors$features[[1]]$geometry$coordinates
  }else{
    point <- NA
  }
  getCoordORS <- point

}

#' Funzione che restituisce le cordinate di un indirizzo utilizzando il servizio mappe di regione toscana nel caso di non riuscita utilizza OpenRuoteService
#' @param indirizzo
#' @param civico
#' @param comune
#' @param provincia
#' @param siglaprovincia
#' @url ULR utilizzata dal servizio della Regione Toscana
#' @param TOKEN
#' @examples
#' indirizzo <- 'via di novoli'
#' civico <- '26'
#' comune <- 'firenze'
#' provincia <- 'Firenze'
#' siglaprovincia <- 'FI'
#' URLRT <- 'http://mappe.regione.toscana.it/legacy-musume/services/Musume?method=richiesta&algoritmo=1&'
#' token <- '5b3ce359785111000we624823fe6724fd45c3681e64432745a3bec'
#' coord <- getCoordinate(indirizzo,civico,comune,provincia,siglaprovincia,url,token)
#'
#' @return vettore con longitudie e latitudine
#' NA se l'indirizzo non viene decodificato
#' @export

getCoordinate<-function(indirizzo,civico,comune,provincia,siglaprovincia,url,token){
  RTcoord=getCoordRT(indirizzo,civico,comune,siglaprovincia,url)
  if (RTcoord[3]=="NOCOORD" || RTcoord[3]=="NOOKResponse"){
    getCoordinate <- getCoordORS(indirizzo,civico,comune,provincia,token)
  }
  else{
    getCoordinate <- RTcoord
  }

}
