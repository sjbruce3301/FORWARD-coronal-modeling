function tomo_speedfit,parms,ht_asympt=ht_asympt,rinrs=rinrs,dens=dens

  moddens=(parms[0]/(1-exp(-((rinrs-1)/parms[1]))))*((ht_asympt/rinrs)^2)
  score=abs(moddens-dens)/dens

  return,score

end