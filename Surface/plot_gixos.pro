Function Plot_gixos, s_0, s_1, _extra = _e

	on_error, 1

	Spec_file_check, Arrpack(s_0,s_1), par= 'Det_th', /pil, lis= wnum, nsc= n
	bet = reform((Scan_field_read(wnum,'angs'))[*,1])
	rbet = Fltround(bet,dig=3)
	sor = Sorpurge(rbet,net=nn)
	if nn mod 2 eq 0 then begin
		sbet = rbet[sor]
		for i =0, nn/2 -1 do begin
			bind = where(rbet eq sbet[2*i])
			sind = where(rbet eq sbet[2*i+1])
			if i eq 0 then begin
				bnum = wnum[bind]
				snum = wnum[sind]
			endif else begin
				bnum = [bnum,wnum[bind]]
				snum = [snum,wnum[sind]]
			endelse
		endfor
	endif else message, "Numbers of signals and backgrounds don't agree!" 

	sfram = total((Scan_field_read(snum,'ncr'))[*,1])
	bfram = total((Scan_field_read(bnum,'ncr'))[*,1])
	coe = [1.,-1.*sfram/bfram]

	Plvar_keep, act = 'sav'
	owin = !d.window
	window,10,xsize=700,ysize=900
	!p.multi=[0,2,2]
	xtit = 'Q!dz!n'

	Pd_patch_z, snum, res=sig, /ylog, /bad, $
	tit = 'Signal, Scan #: ' + Range_comp(snum), _extra = _e
	Pd_patch_z, bnum, res=bac, /ylog, /bad, $
	tit = 'Backgound, Scan #: ' + Range_comp(bnum), _extra = _e
	Scan_show, sig, bac, lcol= [!pcol.red,!pcol.blue], /ylog, xtit= xtit, $
	_extra = _e
	
	zsig = [min(sig[0,*],max=max),max]
	zbac = [min(bac[0,*],max=max),max]
	zran = [zsig[0] > zbac[0], zsig[1] < zbac[1]]

	dif = Scan_lc(sig, bac, coef=coe,/inter)
	dum = where((dif[0,*] ge zran[0]) and (dif[0,*] le zran[1]) and (dif[1,*] gt 0))
	dif = dif[*,dum]

	lohi = alog10([min(sig[1,*],max=max),max])
	yran = 10.^[floor(lohi[0]),ceil(lohi[1])]
	Scan_show, sig, dif, lcol= [!pcol.black,!pcol.purple], /ylog, xtit= xtit, $
	yran= yran, _extra=_e
	!p.multi=0
	if owin ge 0 then wset, owin
	Plvar_keep, act = 'res'

	return,dif
	end