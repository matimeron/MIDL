Function Au_iface, q, fraction = frc, energy = ene, thi = thi, $
	show = sho, wai = wai, _extra = _e

	on_error, 1

	shofl = keyword_set(sho)
	wwai = Default(wai,1)
	rf = Reflect(q=q,ene=ene,elem=['c','h','cl'],num=3,wei=[2,4,2],den=1.253,$
	s_elem=['h','o'],s_wei=[2,1],s_den=1,/form,_extra=_e)

	rorf = 0.*frc

	q[0] = q[0] > Toler()
	for i = 0, n_elements(frc) - 1 do begin
		r =  Reflect(q=q,ene=ene,elem=['au','h','o','c','h','cl'],num=[3,3],$
		wei=[19.32/197*frc[i],[2,1]/18.*(1-frc[i]),2,4,2],$
		den=[19.32*frc[i]+1-frc[i],1.253],thi=thi,$
		s_elem=['h','o'],s_wei=[2,1],s_den=1.,/form,_extra=_e)
		rat = r/rf
		if shofl then begin
			plot, q, rat, _extra = _e
			wait, wwai
		endif
		rorf[i] = max(rat)
	endfor

	return, FPU_fix(rorf)
end