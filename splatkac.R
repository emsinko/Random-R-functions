library(lubridate)


days_between <- function(day_from,day_to, inclusive = FALSE){
  if(!inclusive) days_between(day_from,day_to, inclusive = TRUE) - 1 
  else  length( seq(from = as.Date(day_from), to = as.Date(day_to), by='day'))
}

number_of_month_payments <- function(day_from,day_to ){
  length( seq(from = as.Date(day_from), to = as.Date(day_to), by='month')) 
}


vytvor_splatkovy_kalendar <- function(datum_cr = as.Date('2019-02-28'),datum_cerpania,datum_splatnosti, datum_prvej_splatky, splatka, poskytnuta_ciastka, urokova_sadzba){
   
  datum_splatnosti <- as.Date(datum_splatnosti)
  
  istina_pred_splatkou <- poskytnuta_ciastka * ( 1+ days_between(datum_cerpania, datum_prvej_splatky, inclusive = F) / 365 * urokova_sadzba)
  istina_po_splatke <- istina_pred_splatkou - splatka
  datum_splatky <- as.Date(datum_prvej_splatky)
  urok <- istina_pred_splatkou - poskytnuta_ciastka
  umor <- splatka - urok 
  
  splatkovy_kalendar <- 
    data.frame(datum_cr, 
               datum_cerpania, 
               datum_prvej_splatky,
               poskytnuta_ciastka,
               urokova_sadzba,
               istina_pred_splatkou,
               splatka,
               urok, 
               umor, 
               istina_po_splatke,
               datum_splatky)
  OK <- TRUE
  
  while(OK){
    istina_pred_splatkou <- istina_po_splatke *  (1 + lubridate::days_in_month(datum_splatky) / 365 * urokova_sadzba)
    istina_po_splatke <- istina_pred_splatkou - splatka
    urok <- istina_pred_splatkou - poskytnuta_ciastka
    umor <- splatka - urok 
    month(datum_splatky) <- month(datum_splatky) + 1
    
    splatkovy_kalendar <- 
      rbind(splatkovy_kalendar,
                 datum_cr, 
                 datum_cerpania, 
                 datum_prvej_splatky,
                 poskytnuta_ciastka,
                 urokova_sadzba,
                 istina_pred_splatkou,
                 splatka,
                 urok, 
                 umor, 
                 istina_po_splatke,
                 datum_splatky)
    
    if(datum_splatky >= datum_splatnosti) OK <- FALSE
  }
  
  #splatkovy_kalendar
  
  
  #print(istina_pred_splatkou)
  #print(days_between(datum_cerpania, datum_prvej_splatky))
}

vytvor_splatkovy_kalendar(
  datum_cerpania = '2019-02-05', 
  datum_splatnosti = '2049-02-20',
  datum_prvej_splatky =  '2019-03-20', 
  splatka = 208.53, 
  poskytnuta_ciastka = 63000, 
  urokova_sadzba = 1.2 / 100)
  
  
  
  
  
  
  
  
  
  
  

amortization_schedule <- function(principal, amortization_amount, interest_rate, first_payment, last_payment){
  number <- 1
  period <- number_of_month_payments(day_from = first_payment,day_to = last_payment)
  balance <- principal
  schedule <- data.frame(name = c(), amortization_amount= c(), interest= c(), principal= c(), balance= c())
  #print(period)
  
  while(number <= period){
    interest <- balance * interest_rate
    principal <- amortization_amount - interest
    balance <- balance - principal
    #if(balance - (amortization_amount - (balance * interest_rate)) > 0 ){
    if(balance > 0){ 
      schedule <- rbind(schedule, c(number, amortization_amount, interest, principal, balance)) 
    } else {
      schedule <- rbind(schedule, c(number, amortization_amount, interest, principal, balance)) 
      #schedule[number, ] <- c(number, amortization_amount, interest, principal, balance  )
    }
    number <- number + 1
  }
  colnames(schedule) <- c("n","splatka", "urok", "umor", "istina")
  if(schedule[number - 1,"istina"] > 0) warning("istina neklesla pod nulu")
  schedule[number - 1, ] <- c(number - 1, amortization_amount + balance,  interest, schedule[number - 2, "istina"], 0)  # uprava na konecnych 0 € 
  print(schedule)
}

select * 
from P_SPLUv
where 1 = 1 
    and ID_UCETUV = 200904133  
    and '2019-06-30' BETWEEN  datum_od and datum_do
order by datum_spl
