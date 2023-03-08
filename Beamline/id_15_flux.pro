Function ID_15_flux, ene, harmonic= har, period= per, current= cur, slit= sli,$
	station= stt, mono= mon, air= air, no_mirror= nom, display= dis, file= fil,$
	_extra = _e

;+
; NAME:
;		ID_15_FLUX
; VERSION:
;		8.72
; PURPOSE:
;		Provides theretical flux estimates for 15ID stations.
; CATEGORY:
;		Beamline specific.
; CALLING SEQUENCE:
;		Result = 15_ID_FLUX( ENE, HARMONIC = HAR [, keywords])
; INPUTS:
; 	ENE
; 		Beam energy in keV, scalar or vector.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 	HARMONIC
; 		Specifies the undulator harmonic(s) for which the evaluation is to be
; 		performed.  Possible input forms are:
;
;  		1)	Integer scalar - Taken as the harmonic number.  Must be odd.
;  		2)	2-element vector - In this case all the odd harmonics between the
;  			minimum and maximum of the input will be used.
;  		3)	A vector with 3 or more elements - all the values will be used as
;  			harmonic numbers.  Again, all must be odd.
;  	PERIOD
;  		Undulator period, in mm.  If not given, default value is 33 for current
;  		APS, or 28 for APSU.
;	CURRENT
;		APS current, either in amps or miliamps (doesn't matter which).
;		Default is 100mA.
;	STATION
;		Accepts character value, 'B', 'C' or 'D'.  If no input provided, the
;		default is 'C'.
;	CURRENT
;		APS current, either in amps or miliamps (doesn't matter which).
;		Default is 100mA.
;	SLIT
;		2-element vector, size of the high heatload slit, in mm.  Default is 2x1
;	MONO
;		Numeric scalar (or array, as only the first value is used) indicating
;		the monochromator crystal to be used.  Value of 1 stands for (111), 3
;		stands for (311).
;		Note:	By default, the mono crystal is set internally, (111) for all
;				energies *up to and including* 30 keV, (311) for all energies
;				*above* 30 keV.  The keyword MONO can be used to override this
;				default.  No checking of whether the usage is geometrically
;				feasible is done in this case.
;	AIR
;		Scalar, the length (in cm) of the airpath.  Defaults are set internally
;		for each station.
;	/NO_MIRROR
;		Switch.  If set, the evaluation is for no mirrors in beam path.
;	/DISPLAY
;		Switch.  If set, the result(s) are displayed to the screen in tabular
;		form.
;	FILE
;		Can be used either as a switch, or with explicit character input
;		translating to a file name.  Causes the tabular display to be saved
;		into a file.  If provided as a switch, a dialog will open asking the
;		user to provide the file name.
;		Note:	FILE implies DISPLAY, automatically.
;	_EXTRA
;		A formal keyword for transfering keywords to embedded routines.  Not to
;		be used directly.
; OUTPUTS:
;		Returns the evaluated flux values for all energies and harmonics in
;		input as a 2D array, with dimensions of [# of harmonics, # of energies].
;		If only a single harmonic provided, returns a 1D vector with same length
;		as ENE.  If single harmonic and single energy is provided, returns a
;		scalar output.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		All the harmonic values must be odd.
; PROCEDURE:
;		Calls UND_FLUX from SRUFF to evaluate raw flux.  Calls REF_CURVE from
;		MONO_LIB and MIRROR from SRUFF_LIB to assess monochromator and mirror
;		reflection losses, then calls ABS_COEFF from SRUFF_LIB to correct result
;		for absorption.  Also calls CALCTYPE, CAST, DEFAULT, ISNUM, STRMATCH_MM
;		and TABULATE, from MIDL.
; MODIFICATION HISTORY:
;		Created 5-FEB-2016 by Mati Meron.
;		Modified 20-AUG-2016 by Mati Meron.  Added keywords MONO and AIR.
;		Modified 10-OCT-2016 by Mati Meron.  Added keyword PERIOD.
;		Modified 20-DEC-2020 by Mati Meron.  Internal changes.
;-

	on_error, 1

	postt = ['B','C','D']
	if keyword_set(stt) then begin
		ist = Strmatch_mm(stt,postt)
		if ist lt 0 then message, 'Accepted stations are B, C and D only!'
	endif else ist = 1

	elim = [0,10,20,30,50,100]
	nlim = n_elements(elim)-1
	monlim = [0.,30]
	crys = 'si'
	cind = [[1,1,1],[1,1,1],[1,1,1],[3,1,1],[3,1,1]]
	bw = [1.324e-4,1.324e-4,1.324e-4,2.773e-5,2.773e-5]
	if keyword_set(mon) then begin
		case mon[0] of
			1	:	begin
						cind[0,*] = 1
						bw = replicate(bw[0],nlim)
					end
			3	:	begin
						cind[0,*] = 3
						bw = replicate(bw[-1],nlim)
					end
			else:	message, 'Unrecognized mono crystal!'
		endcase
	endif

	mirel = ['si','rh','pt','pt','pt']
	miran = [2,2,2,1.4,1]*1e-3
	mirof = 3
	mirlen = 600.
	mirdist = 32.5
	mirfl = not keyword_set(nom)

	bethi = 2e-2*2.54

	kapel = ['h','c','n','o']
	kapwei = [10,22,2,5]
	kapden = 1.42
	kapthi = ([5,15,15]*2.54e-3)[ist]

	airel = ['n','o','ar']
	airwei = [78,21,1]
	airden = 1.2e-3
	airthi = Default(air,([25.,40,50])[ist],/dtyp)

	if not Isnum(per) then begin
		case !blpar.rene of
			6	:	wper = 28
			7	:	wper = 33
			else:	message, 'Unsupported case!'
		endcase
	endif else wper = per
	if Isnum(cur) then begin
		if cur gt 1 then wcur = 1e-3*cur else wcur = cur
	endif

	ape = Default(sli,[2,1.],/dtyp)
	if n_elements(ape) ne 2 then message, 'Slit needs 2 values!
	dist = 28.

	nene = n_elements(ene)
	if nene gt 0 then begin
		typ = Calctype(0.,ene)
		nhar = n_elements(har)
		case nhar of
			0	:	message, 'Missing harmonic input!'
			1	:	whar = har
			2	:	begin
						lo = min(har,max=hi)
						hlo = floor(lo)/2*2+1
						hhi = (ceil(hi)-1)/2*2 + 1
						whar = hlo + 2*lindgen((hhi-hlo)/2+1)
					end
			else:	whar = har
		endcase
		nhar = n_elements(whar)

		res = make_array(nhar,nene,typ=typ)
		monref = make_array(nene,typ=typ)
		mirref = monref + 1

		for i = 0, nhar-1 do begin
			for j = 0, nlim-1 do begin
				dum = where(ene gt elim[j] and ene le elim[j+1], ndum)
				if ndum gt 0 then begin
					wape = [ape[0],ape[1] < mirlen*miran[j]*dist/mirdist]
					res[i,dum] = $
					Und_flux(ene[dum],per=wper,har=whar[i],/quiet,band=bw[j],$
					/def,cur=wcur,/cor,/opt,ape=wape,dist=dist,_extra=_e)
					if i eq 0 then begin
						monref[dum] = $
						Ref_curve(0,ene=ene[dum],crys=crys,ind=cind[*,j])
						if mirfl then mirref[dum] = $
						Mirror(ene[dum],miran[j],ele=mirel[j],rough=mirof)
					endif
				endif
			endfor
		endfor
		beabs = Abs_coeff(ene,elem='be')
		kapabs = Abs_coeff(ene,elem=kapel,wei=kapwei,/form,den=kapden)
		airabs = Abs_coeff(ene,elem=airel,wei=airwei,/form,den=airden)
		trans = exp(-bethi*beabs-kapthi*kapabs-airthi*airabs)
		mfac = monref^2*mirref^2*trans
		for j = 0, nhar-1 do res[j,*] = res[j,*]*mfac

		if keyword_set(dis) or keyword_set(fil) then begin
			nco = 10
			sind = strcompress(sindgen(nco),/rem)
			clm = 'c' + sind
			for k = 0, (nco-1) < (nhar-1) do $
			dum = execute(clm[k]+'= res['+sind[k]+',*]')
			tit = '15ID' + postt[ist] + ' Flux'
			if not mirfl then tit = tit + ' (no mirrors)'
			head = ['E (keV)', 'Har #' + string(whar,form='(i0)')]
			form = replicate('g10.3',nco+1)
			wid = 12*((nco < nhar) + 1)
			print
			Tabulate, ene, c0, c1, c2, c3, c4, c5, c6, c7, c8, c9,$
			tit= tit, head= head, form= form, width = wid
			if keyword_set(fil) then begin
				if Isnum(fil) then fil = ''
				Tabulate, ene, c0, c1, c2, c3, c4, c5, c6, c7, c8, c9,$
				tit= tit, head= head, form= form, width = wid, file = fil
			endif
		endif
	endif else message, 'Missing energy input!'

	if nhar eq 1 then begin
		if nene eq 1 then res = res[0] else res = reform(res)
	endif

	return, Cast(res,typ,typ)
end