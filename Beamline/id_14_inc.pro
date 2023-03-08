Function ID_14_inc, loc, focloc = foc, hang = han, vang = van, air = air, $
	show = sho, win = wwin, spot = spot, stat = stat, out = pow, _extra = _e

	common id_14_pars, exs, milfac, micfac, elem, eloc, $
	filt, flth, wind, wnth, mircot, mirlen, mirrad, mirser, $
	aper, curap, filf, fils, curcnf, sunfl

	on_error, 1
	if Type(exs) eq 0 then ID_14_gen
	eps = 1e-3

	if Type(loc) eq 7 then begin
		dum = Strmatch_mm(loc,elem,3)
		if dum ge 0 then wloc = eloc[dum] else message, 'Unknown location!'
	endif else if Isnum(loc) then wloc = loc else message, 'Unknown location!'
	if Type(foc) eq 7 then begin
		dum = Strmatch_mm(foc,elem,3)
		if dum ge 0 then wfoc = eloc[dum] else message, 'Unknown location!'
	endif else wfoc = Default(foc,wloc)

	aloc = eloc[Strmatch_mm('apert',elem)]
	mloc = eloc[[Strmatch_mm('hmirr',elem),Strmatch_mm('vmirr',elem)]]
	if Isnum(han) then hfl = ((han gt eps) and (wloc gt mloc[0])) else hfl = 0b
	if Isnum(van) then vfl = ((van gt eps) and (wloc gt mloc[1])) else vfl = 0b

	wwin = (awin = 0.5*aper)
	if hfl then wwin[0] = wwin[0] < 0.5*han*mirlen*aloc/mloc[0]
	if vfl then wwin[1] = wwin[1] < 0.5*van*mirlen*aloc/mloc[1]

	if wloc gt eloc[Strmatch_mm('windo',elem)] then begin
		wfilt = [filt,wind]
		wflth = [flth,wnth]
	endif else begin
		wfilt = filt
		if wloc lt eloc[Strmatch_mm('apert',elem)] $
		then wflth = 0. else wflth = flth
	endelse

	if keyword_set(air) then begin
		sloc = eloc[Strmatch_mm('sampl',elem)]
		if wloc gt sloc then begin
			wfilt = [wfilt,['n','o','ar']]
			wflth = [wflth,milfac*(wloc-sloc)*[0.78,0.21,0.01]]
		endif
	endif

	case (hfl + 2*vfl) of
		0	:	begin
					un_show, file = filf,.1, aloc, win = wwin, /sme, $
					filte = wfilt, filth = wflth, /no_sh, out = farf
					un_show, file = fils,.1, aloc, win = wwin, /sme, $
					filte = wfilt, filth = wflth, /no_sh, out = fars
				end
		1	:	begin
					un_show, file = filf,.1, aloc, win = wwin, /sme, $
					filte = wfilt, filth = wflth, /no_sh, out = farf, $
					mirr = mircot, miran = han
					un_show, file = fils,.1, aloc, win = wwin, /sme, $
					filte = wfilt, filth = wflth, /no_sh, out = fars, $
					mirr = mircot, miran = han
				end
		2	:	begin
					un_show, file = filf,.1, aloc, win = wwin, /sme, $
					filte = wfilt, filth = wflth, /no_sh, out = farf, $
					mirr = mircot, miran = van
					un_show, file = fils,.1, aloc, win = wwin, /sme, $
					filte = wfilt, filth = wflth, /no_sh, out = fars, $
					mirr = mircot, miran = van
				end
		3	:	begin
					un_show, file = filf,.1, aloc, win = wwin, /sme, $
					filte = wfilt, filth = wflth, /no_sh, out = farf, $
					mirr = [mircot,mircot], miran = [han,van]
					un_show, file = fils,.1, aloc, win = wwin, /sme, $
					filte = wfilt, filth = wflth, /no_sh, out = fars, $
					mirr = [mircot,mircot], miran = [han,van]
				end
	endcase

	if Streq(curap,'huge',3) then arfac= 1. else arfac= Net_area(wwin,awin,/rel)
	pow = farf
	pow[2,*,*] = (farf[2,*,*] + fars[2,*,*])*arfac/(1+ sunfl)
	drat = wloc/aloc
	wwin = drat*wwin
	x = drat*reform(pow[0,*,*])
	y = drat*reform(pow[1,*,*])
	p = (1./drat)^2*reform(pow[2,*,*])
	frame = Winframe(x,y,window=wwin,spac=dxy,windex=qxy,fringe=fri)
	res = Partot(p,syme=qxy,symf=fri)
	spot = ID_14_spot(wloc,foc=wfoc,hang=han,vang=van,stat=stat)
	if arg_present(pow) or keyword_set(sho) then begin
		wcheck = spot ge 2*wwin
		tem = dxy/spot
		if hfl or wcheck[0] then tem[0] = Partot($
		(Partot(p,2,syme=qxy[1],symf=fri[1])/res)^2,syme=qxy[0],symf=fri[0])
		if vfl or wcheck[1] then tem[1] = Partot($
		(Partot(p,1,syme=qxy[0],symf=fri[0])/res)^2,syme=qxy[1],symf=fri[1])
		rat = spot/dxy*tem
		wwin = rat*wwin
		nxy = (size(p))[1:2]
		x = rat[0]*Winsnip(nxy,qxy,x,/full)
		y = rat[1]*Winsnip(nxy,qxy,y,/full)
		p = 1/(rat[0]*rat[1])*Winsnip(nxy,qxy,p,/full)
		pow = transpose([[[x]],[[y]],[[p]]],[2,0,1])
	endif
	res = dxy[0]*dxy[1]*res

	if keyword_set(sho) then Display_mm, pow, /auz, /rest, _extra = _e

	return, res
end