Function TUND_make, ene, taper = tap, $
	period= per, harm= har, eharm= ehr, bandwidth= ban, def= def, scu= scu, $
	nper= npr, und_length= udl, ring_energy= ren, rgam= rgm, current= cur, $
	radial_size= rsg, angular_size= asg, fast= fst, progress= prg, show= sho, $
	save=sav, _extra =_e

;+
; NAME:
;		UND_BRIGHTNESS
; VERSION:
;		8.46
; PURPOSE:
;		Calculates the on-axis brightness of a tapered undulator source.
; CATEGORY:
;		X-ray specific.
; CALLING SEQUENCE:
;		Result = UNDT_BRIGHTNESS( ENE, PERIOD = PER [,keywords])
; INPUTS:
;	ENE
;		Numeric scalar or vector, photon energy in keV units.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 	GTET
; 		Scalar, the value of gamma*theta (standard undulator meaning.  Default
; 		is 0.
; 	PHI
; 		Scalar, the value of the Phi angle (standard undulator meaning.  Default
; 		is 0.
;	TAPER
;		Scalar, the taper value, expressed as the fractional change of the field
;		(not the gap) from one end of the undulator to the other.  Default is 0.
;	PERIOD
;		The length of the undulator period.  If the value is less then 1, it is
;		assumed to be in meters, else in milimeters.  Mandatory, no defaults.
;	HARM
;		Integer type, The undulator harmonic number.  Only odd numbers are
;		accepted.  Default is 1.
;	EHARM
;		Scalar, the photon energy (in keV) at the harmonic (HARM) for 0 taper.
;	BANDWIDTH
;		The bandwidth used in calculation.  Default is 1e-3.
;	/DEF
;		Switch.  If set and some inputs are missing, they're replaced by
;		defaults from !BLPAR.
;	/SCU
;		Switch.  If set, the evaluation is for a superconducting undulator.
;	NPER
;		Number of periods.  If not given, calculated	|	At most one of
;		from period and UND_LENGTH.						| 	these two may
;	UND_LENGTH											|	be given.
;		The length of the undulator in meters.  If		|
;		not given calculated from PERIOD and NPER or	|
;		(if NPER is also absent but /DEF is set) 		|
;		replaced by the default value in !BLPAR.
;	RING_ENERGY									|	At most one of these two may
;		Synchrotron energy, in GeV.   			|	be given.  If none is and
;	RGAM										|	/DEF is set value is taken
;		The relativistic Gamma of the source.	|	from !BLPAR.
;	CURRENT
;		Source electron current, in Amp.  If not given and /DEF is set, value
;		is taken from !BLPAR.
;	RADIAL_SIZE
;		Source spatial size in the format of [horizontal_sigma,vertical_sigma].
;		If not given and /DEF is set, values are taken from !BLPAR.
;	ANGULAR_SIZE
;		Source angular size in the format of [horizontal_sigma,vertical_sigma].
;		If not given and /DEF is set, values are taken from !BLPAR.
;	/CORRECT
;		Switch.  If set, higher order corrections to radiative spatial and
;		angular width are calculated.  This has a very small effect, except for
;		very high harmonics.
; OUTPUTS:
;		Returns the on-axis brightness of the undulator, for the energy values
;		provided, with the undulator being set to the energy EHAR (for the 
;		harmonic given by HARM).  The output is of same length and form as the 
;		input ENE.  The units are photons/sec/mr^2/bw, where bw is the bandwidth
;		being used.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The energy must be no higher than the saturation energy for the
;		undulator, at the given harmonic.
; PROCEDURE:
;		Evaluates the Alferov integral for synchrotron radiated power.  Calls
;		BL_DEFAULTS, ID_CONV, UND_BEAMSIZE and UND_INT.  Calls CAST, DEFAULT,
;		FLTROUND, HOW_MANY, NULLIFY, ONE_OF, TOLER and TYPE, from MIDL.
; MODIFICATION HISTORY:
;		Created 5-MAR-2016 by Mati Meron as a variation on the UND_BRIGHTNESS
;		routine.
;-

	on_error, 1
	eps = Toler()
	mult = 1d3

	typ = Type(ene)
	tap = Default(tap,0.,/dtyp)
	har = Default(har,1,/dtyp)
	ban = Default(ban,1e-3,/dtyp)
	nrfl = keyword_set(nrm)
	if not (har mod 2) then message, 'Odd harmonics only accepted!'
	dum = How_many(fir=udl,whi=whi)

	if n_elements(per) eq 1 then begin
		if per lt 1 then wper = mult*per else wper = 1d*per
		case One_of(npr,udl) of
			-1	:
			0	:	udl = Fltround(wper*npr/mult,dig=2)
			1	:	udl = 1d*udl
		endcase
		if keyword_set(def) then BL_Defaults, dev= udl, min_gap = mgp, $
		radial= rsg, angular= asg, ene= ren, gamm= rgm, curr= cur, scu= scu
		wnpr = Default(npr,floor(mult*udl/wper),/dtyp)

		ID_conv, per= wper, gap= mgp, ene= emn, emax= emx, scu= scu, _extra= _e
		emn = har*emn
		emx = har*emx
		wehr = Default(ehr,min(ene))
		erat = ene*har/wehr
		if wehr lt emn*(1 - 4*eps) or wehr gt emx*(1 + 4*eps) $
		then message,'Energy out of range for this harmonic!'
		k = sqrt(2*((emx/wehr) - 1) > 0)
		w = k/(1 + k^2/2)

		gtsg = rgm*asg/mult
		utsg = sqrt((1 + k^2/2)/(4*wnpr*har))
		step = (gtsg < utsg)/(4 - 2*keyword_set(fst))
		nxy = ceil(4*(sqrt(gtsg^2 + utsg^2))/step)
		limxy = nxy*step
		nen = n_elements(ene)
		dim = [nen,2*nxy+1]
		dat = make_array(dim,/doub)
		nrmdat = make_array(dim[1:2],/doub)
		txy = make_grid([-1,1]#limxy,2*nxy+1)
		gte = sqrt(total(txy^2,1))
		phi = acos(reform(txy[0,*,*])/(gte > Toler()))

		if prg then begin
			print
			nevl = 1.*(nxy[0] + 1)*(nxy[1] + 1)
			prfl = 1
			form = ['("	",i0,"% done"','"	",f7.2," sec. elapsed"', $
				'"	",f7.2," sec. remaining (est.)")']
			form = strjoin(form,',')
			t = systime(1)
		endif else prfl = 0

		for i = 0, nxy[0] do begin
			for j = 0, nxy[1] do begin
				dat[*,i,j] = (dat[*,2*nxy[0]-i,j] = (dat[*,i,2*nxy[1]-j] = $
				(dat[*,2*nxy[0]-i,2*nxy[1]-j] = UND_int(erat,$
				k=k,gte=gte[i,j],phi=phi[i,j],nper=wnpr,tap=tap,acc='lo'))))
				nrmdat[i,j] = (nrmdat[2*nxy[0]-i,j] = $
				(nrmdat[i,2*nxy[1]-j]= (nrmdat[2*nxy[0]-i,2*nxy[1]-j] = $
				UND_int(har,k=k,gte=gte[i,j],phi=phi[i,j],nper=wnpr,acc='lo'))))
				if prfl then begin
					tem = i*(nxy[1] + 1) + j
					frac = (tem + [0,1])/nevl
					check = floor(10.*frac)
					if check[0] ne check[1] then begin
						dt = systime(1) - t
						print, 10*check[1], dt, dt*(1/frac[1]-1), form=form
					endif
				endif
			endfor
		endfor

		dat = !srcon.alp*!srcon.scal*cur*ban/mult*(wnpr*rgm*w)^2*dat
		nrmdat = !srcon.alp*!srcon.scal*cur*ban/mult*(wnpr*rgm*w)^2*nrmdat

		udat = {tund_dat}
		udat.sync = !blpar.desc
		udat.per = wper
		udat.npr = wnpr
		udat.tap = tap
		udat.harm = har
		udat.eharm = wehr
		udat.bw = ban
		udat.rgam = rgm
		udat.cur = cur
		udat.rsig = rsg
		udat.asig = asg
		udat.ene = ptr_new(ene)
		udat.gtxy = ptr_new(Cast(txy,4,typ))
		udat.dat = ptr_new(Cast(dat,4,typ))
		udat.nrmdat = ptr_new(Cast(nrmdat,4,typ,/fix))

		if keyword_set(sav) then begin
			if Type(sav) eq 7 then begin
				fnam = getenv('sruff_data') + sav + $
				string(wehr,har,100*tap,form='("_e",i0,"h",i0,"t",i0,".sav")')
				save, udat, file = fnam, _extra = _e
			endif else save, udat, file =File_get(/write,def='sav'), _extra= _e
		endif

		if keyword_set(sho) then begin
			Tund_show, udat, _extra = _e
			wait, 0.1
		endif

		Nullify, whi, fir=udl
	endif else message, 'Missing Period length!'

	return, udat
end