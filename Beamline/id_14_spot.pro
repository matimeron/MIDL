Function ID_14_spot, loc, ene, focloc = foc, hang = han, vang = van, $
	kval = kvl, slope_error = ser, $
	sigma = sig, fwhm = fwh, nof_res = nfr, stat = stat

	common id_14_pars, exs, milfac, micfac, elem, eloc, $
	filt, flth, wind, wnth, mircot, mirlen, mirrad, mirser, $
	aper, curap, filf, fils, curcnf, sunfl

	on_error, 1
	if Type(exs) eq 0 then ID_14_gen
	eps = 1e-3

	if n_elements(loc) ne 1 then message, 'Bad or missing location!'
	foc = Default(foc,loc)
	ene = Default(ene,12.,/dtyp)
	kvl = Default(kvl,1.,/dtyp)
	wser = Default(ser,mirser,/dtyp)*2*sqrt(4*!pi)/micfac
	if n_elements(wser) eq 1 then wser = [wser,wser]

	aloc = eloc[Strmatch_mm('apert',elem)]
	mloc = eloc[[Strmatch_mm('hmirr',elem),Strmatch_mm('vmirr',elem)]]
	if Isnum(han) then hfl = ((han gt eps) and (loc gt mloc[0])) else hfl = 0b
	if Isnum(van) then vfl = ((van gt eps) and (loc gt mloc[1])) else vfl = 0b

	lam = 1e-10*!srcon.conv/ene
	tasq = (KJK_sigs(kvl,/cor)/!blpar.rgam)^2 + (!blpar.asig/milfac)^2
	wrsq = 4*!pi*((!blpar.rsig/milfac)^2 + 2*lam*!blpar.dlen/(4*!pi)^2)
	wasq = 4*!pi*(tasq + lam/(2*!blpar.dlen))
	htem = [1,wrsq[0],wasq[0],aloc]
	vtem = [1,wrsq[1],wasq[1],aloc]

	nfr = fltarr(2)
	nfr[0] = Hsize(Fprop(lam,Sslit(lam,htem,aper[0]/milfac),loc-aloc))/milfac
	nfr[1] = Hsize(Fprop(lam,Sslit(lam,vtem,aper[1]/milfac),loc-aloc))/milfac
	res = nfr
	stat = intarr(2)

	if hfl then begin
		shan = sin(han/milfac)
		htem = Fprop(lam,Sslit(lam,htem,aper[0]/milfac),mloc[0]-aloc)
		htem = Difus(wser[0],Sslit(lam,htem,mirlen*shan),1.)
		fcl = Focln(htem,foc-mloc[0])
		fclm = mirrad*shan/2
		if fcl lt fclm then begin
			fcl = fclm
			stat[0] = 2
		endif else stat[0] = 1
		htem = Fprop(lam,Focus(lam,htem,fcl),loc-mloc[0])
		res[0] = Hsize(htem)/milfac
	endif
	if vfl then begin
		svan = sin(van/milfac)
		vtem = Fprop(lam,Sslit(lam,vtem,aper[1]/milfac),mloc[1]-aloc)
		vtem = Difus(wser[1],Sslit(lam,vtem,mirlen*svan),1.)
		fcl = Focln(vtem,foc-mloc[1])
		fclm = mirrad*svan/2
		if fcl lt fclm then begin
			fcl = fclm
			stat[1] = 2
		endif else stat[1] = 1
		fcl = Focln(vtem,foc-mloc[1]) > mirrad*svan/2
		vtem = Fprop(lam,Focus(lam,vtem,fcl),loc-mloc[1])
		res[1] = Hsize(vtem)/milfac
	endif

	case One_of(sig,fwh) of
		0	:	begin
					res = res/sqrt(4*!pi)
					nfr = nfr/sqrt(4*!pi)
				end
		1	:	begin
					res = res*sqrt(alog(256)/(4*!pi))
					nfr = nfr*sqrt(alog(256)/(4*!pi))
				end
		else:
	endcase

	return, res
end