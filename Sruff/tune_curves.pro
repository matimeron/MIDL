Pro Tune_curves, eran, period= per, harm_max= hrm, all_harm=all, bandwidth=ban,$
	min_gap = mgp, emin = emi, kmax = kma, nper = npr, und_length = udl, $
	current = cur, ring_energy = ren, rgam = rgm, def = def, scu = scu, $
	radial_size = rsg, angular_size = asg, correct = crr, alternate = alt, $
	color = col, line = lin, thick = thi, numbers = nmb, legend = leg, $
	result = res, _extra = _e

;+
; NAME:
;		TUNE_CURVES
; VERSION:
;		8.45
; PURPOSE:
;		Calculates the tune curves of a synchrotron undulator source.
; CATEGORY:
;		X-ray specific.
; CALLING SEQUENCE:
;		TUNE_CURVES, ERAN, PERIOD = PER [, keywords]
; INPUTS:
;	ENE
;		Photon energy range, in keV.  Can be given as:
;			1) 	Scalar.  In this case the value is assumed to be the top of the
;				range, with 0 being the bottom, and an internal array with
;				with values in the [bottom,top] range with a step of 0.1 keV
;				is generated.
;			2)  A 2 element vector, taken as a [bottom,top] range.  An internal
;				array in this range with a step of 0.1 keV is generated,
;			Other):	if number of elements is 0, and error occurs else the input
;					is taken as a set photon of energy values for calculation.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	PERIOD
;		Scalar or vector, the length(s) of the undulator period(s).  If the
;		value(s) is (are) less then 1, lenght in meters is assumed, else in
;		milimeters.  Mandatory, no defaults.
;	HARM_MAX
;		Integer type, The undulator maximal harmonic number.  Only odd numbers
;		are accepted.  Default value is 1.  all odd harmonics in the range
;		[1,HARM_MAX] are evaluated.
;	/ALL_HARM
;		Switch.  If set, all the harmonics that fall within the provided energy
;		range are evaluated.  HARM_MAX is ignored in this case.
;	BANDWIDTH
;		The bandwidth used in calculation.  Default is 1e-3.
;	MIN_GAP
;		Minimal gap possible on the undulator.  If not given and /DEF (see
;		below) is set, MIN_GAP takes the value from !BLPAR.
;	EMIN										|
;		Scalar, minimal first harmonic energy.	|	No more than one of these
;	KMAX										|	two keywords may be used.
;		Scalar, maximal K value to use			|
;
;		Note:	By default, minimal energy or maximal K (these are exchangeable
;				is set by the value of MIN_GAP.  If, instead, EMIN or KMAX is
;				given, it overrides the default value, provided the explicit
;				value falls within the range allowed for the first harmonic.
;				If not, it is ignored.
;	NPER
;		Number of periods.  If not given, calculated	|
;		from period(s) and UND_LENGTH.  If given, must	|
;		have as many entries as PERIOD.					|	At most one of
;	UND_LENGTH											|	these two may
;		The length of the undulator in meters.  If		|	be given.
;		needed and not given, replaced by the default 	|
;		value in !BLPAR (only when /DEF is set). 		|
;	CURRENT
;		Source electron current, in Amp.  If not given and /DEF is set, value
;		is taken from !BLPAR.
;	RING_ENERGY									|	At most one of these two may
;		Synchrotron energy, in GeV.   			|	be given.  If none is and
;	RGAM										|	/DEF is set value is taken
;		The relativistic Gamma of the source.	|	from !BLPAR.
;	/DEF
;		Switch.  If set and some inputs are missing, they're replaced by
;		defaults from !BLPAR.
;	/SCU
;		Switch.  If set, the evaluation is for a superconducting undulator.
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
;	/ALTERNATE
;		Switch.  If set, an envelope of the curves is drawn instead of all the
;		individual curves.
;	COLOR
;		Color code(s).  If given, the number of entries must be equal or greater
;		than this of PERIOD, in which case each consecutive color will be used
;		for all the tune curves of one period length.
;	LINE
;		Same as color, for line type.
;	THICK
;		Same as color, for line thickness.
;	/NUMBERS
;		Switch.  If set, the harmonic numbers are printed to the plot next to
;		the corresponding curves.  The number always uppears near the leftmost
;		(lowest energy) point of each curve.
;	LEGEND
;		Switch.  If set, a legend is added to the plot.
;	RESULT
;		Optional output, see below.
;	_EXTRA
;		A formal keyword for transfering keywords to embedded routines.  Not to
;		be used directly.
; OUTPUTS:
;		No return other than screen graphic output of tune curves, for the
;		requiered period lengths and harmonics, within the required range.
; OPTIONAL OUTPUT PARAMETERS:
;	RESULT
;		Returns the energy-brilliance curve as a [2,N] array.  Active only when
;		/ALTERNATE is set and the period input is a scalar.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Calculates from the mathematical form of the on-axis distribution,
;		correcting for finite source size (spatial and angular).  Calls
;		BL_DEFAULTS, ID_CONV and UND_BRILLIANCE.  Calls DEFAULT, LABELS,
;		LEGEND_MM, MAKE_GRID, ONE_OF and WHERINSTRUCT, from MIDL.
; MODIFICATION HISTORY:
;		Created 30-DEC-2006 by Mati Meron.
;		Modified 1-APR-2007 by Mati Meron.  Added keyword /DEF.
;		Modified 5-JAN-2013 by Mati Meron.  Added keywords CORRECT, ALTERNATE,
;		NUMBERS and LEGEND.
;		Modified 25-OCT-2015 by Mati Meron.  Added keyword SCU for
;		superconducting undulator option.  Added keyword RESULT.
;		Modified 10-JAN-2016 by Mati Meron.  Added keyword ALL_HARM.
;		Modified 20-JAN-2016.  Added keywords EMIN and KMAX.
;-

	on_error, 1
	mult = 1e3
	step = .01

	ban = Default(ban,1e-3,/dtyp)
	btem = byte(string(1e2*ban,form='(f6.4)'))
	dum = where(btem ne 48, ndum)
	if ndum gt 0 then stem = string(btem[0:dum[ndum-1]]) + '%bw' $
	else message, 'Invalid bandwidth!'
	xtit = 'Energy (keV)'
	ytit = 'On-axis Brilliance (ph/s/mrad!u2!n/mm!u2!n/' + stem + ')'

	case n_elements(eran) of
		0	:	message, 'Missing energy range!'
		1	:	ene = Make_grid([0,eran],step, /step)
		2	:	ene = Make_grid(eran,step,/step)
		else:	ene = eran
	endcase

	numper = n_elements(per)
	if min(per) lt 1 then wper = mult*per else wper = per
	case One_of(npr,udl) of
		-1	:	begin
					if keyword_set(def) then begin
						BL_defaults, dev = dlen, scu = scu
						wnpr = fix(mult*dlen/wper)
					endif else message, 'Device length or # of periods needed!'
				end
		0	:	wnpr = npr
		1	:	wnpr = fix(mult*udl/wper)
	endcase

	if n_elements(wnpr) ne numper then message, 'Period-number discrepancies!'
	wcol = Default(col,!pcol.(0))
	if n_elements(wcol) eq 1 then wcol = replicate(wcol,numper) else $
	if n_elements(wcol) lt numper then message, 'Wrong number of colors!'
	wlin = Default(lin,0)
	if n_elements(wlin) eq 1 then wlin = replicate(wlin,numper) else $
	if n_elements(wlin) lt numper then message, 'Wrong number of line types!'
	wthi = Default(thi,1)
	if n_elements(wthi) eq 1 then wthi = replicate(wthi,numper) else $
	if n_elements(wthi) lt numper then message, 'Wrong number of thicknesses!'

	s = sort(wper)
	wper = wper[s]
	wnpr = wnpr[s]
	wcol = wcol[s]
	wlin = wlin[s]
	wthi = wthi[s]

	if keyword_set(def) then $
	BL_Defaults, ene = ren, gamm = rgm, cur = cur, min_gap = mgp, scu = scu
	ID_conv, gap=mgp, per=wper, ene=emn, emax=emx, rgam=rgm, scu=scu, _extra=_e
	stit = !blpar.sync+ ' ;  Ring energy = '+ string(ren,form='(f5.2,"GeV")')+ $
		' ;  Ring current = ' + string(fix(1e3*cur),form ='(i0,"mA")')

	case One_of(emi,kma,val=val) of
		-1	:	val = emn
		0	:
		1	:	val = emx/(1 + val^2/2)
	endcase
	emn = emn > val < emx

	if keyword_set(all) then begin
		rat = max(ene)/min(emn)
		hrm = floor((rat-1)/2)*2 + 1
	endif else begin
		hrm = Default(hrm,3,/dtyp)
		if not (hrm mod 2) then message, 'Odd harmonics only accepted!'
	endelse

	rfl = arg_present(res) and numper eq 1
	nfl = keyword_set(nmb) and numper eq 1
	afl = keyword_set(alt) or rfl
	ylfl = ((Wherinstruct('ylo',_e))[0] ge 0)
	if not ylfl then ymax = 0.
	for i = 0, numper - 1 do begin
		if afl then gbril = 0*ene
		if nfl then ninf = lonarr(2,(1+hrm)/2)
		for har = 1, hrm, 2 do begin
			dum = where(ene ge har*emn[i] and ene lt har*emx[i], ndum)
			if nfl then ninf[*,(har-1)/2] = [dum[0],har]
			if ndum gt 0 then begin
				bril = $
				UND_brilliance(ene[dum],per=wper[i],har=har,ban=ban,def=def,$
				nper=wnpr[i],scu=scu,rgam=rgm,cur=cur,rad=rsg,ang=asg,cor=crr)
				if not ylfl then ymax = ymax > max(bril)
				if not afl then begin
					if (i eq 0 and har eq 1) then plot, ene[dum], bril, /nodat,$
					ymargin= [6,2], xtit= xtit, ytit= ytit,subtit= stit, $
					xran=[min(ene),max(ene)],_extra=_e
					oplot,ene[dum],bril,col= wcol[i],line= wlin[i],thi= wthi[i]
					if nfl then begin
						if ylfl then yoff = 0.02*bril[0] else yoff = 0.02*ymax
						Labels, ene[dum[0]], bril[0] + yoff, align = 0.5, $
						string(har,form='(i0)'), col=!pcol.purple, _extra=_e
					endif
				endif else begin
					tbril = gbril
					tbril[dum] = gbril[dum] > bril
					if nfl then begin
						tloc = where(tbril gt gbril,ntloc)
						if ntloc gt 0 then ninf[0,(har-1)/2] = tloc[0]
					endif
					gbril = tbril
				endelse
			endif
		endfor
		if afl then begin
			if i eq 0 then begin
				dum = where(gbril gt 0)
				if ylfl then begin
					gmax = max(gbril[dum],min=gmin)
					yran = 10.^[floor(alog10(gmin))-1,ceil(alog10(gmax))]
				endif
				plot, ene, gbril, /nodat,ymargin= [6,2],xtit= xtit,ytit= ytit,$
				subtit= stit, xran=[min(ene),max(ene)],yran=yran,_extra=_e
				if rfl then res = transpose([[ene[dum]],[gbril[dum]]])
			endif
			oplot, ene, gbril, col= wcol[i], line= wlin[i], thi= wthi[i]
			if nfl then begin
				dum = where(ninf[0,*] ge 0,ndum)
				if ndum gt 0 then begin
					ind = ninf[0,dum]
					if ylfl then yoff = 0.02*gbril[ind] else yoff = 0.02*ymax
					Labels, ene[ind], gbril[ind] + yoff, align = 0.5, $
					string(ninf[1,dum],form='(i0)'), col = !pcol.purple
				endif
			endif
		endif
	endfor

	if keyword_set(leg) then begin
		ss = sort(s)
		wper = wper[ss]
		wnpr = wnpr[ss]
		wcol = wcol[ss]
		wlin = wlin[ss]
		lthi = max(wthi)
		Legend_mm, text=string(wper,form='(f4.1,"mm")'),$
		line=wlin[0:numper-1],color=wcol[0:numper-1],thi=lthi,ext=2,_extra=_e
	endif

	return
end