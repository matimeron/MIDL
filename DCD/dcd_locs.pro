Pro DCD_locs, erange = ern, step = stp, fixed = fix, length = len, $
	crystal = crs, radians = rad, precise= pre, no_show = nos, $
	ene= ene, ftet= ftet, stet= stet, ful= ful, lon= lon, tra= tra, _extra= _e

;+
; NAME:
;		DCD_LOCS
; VERSION:
;		8.72
; PURPOSE:
;		Evaluates parameters of a Double Crystal Diffractometer (DCD).
; CATEGORY:
;		X-ray optics.
; CALLING SEQUENCE:
;		DCD_LOCS, ERANGE = ERN [, Keywords]
; INPUTS:
;		None.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	ERANGE
;		Two possible modes:
;			1) 	Scalar, used as evaluation energy, in keV.
;			2) 	2-element vector.  Values used as limits of an energy range,
;				with intermediate values being filled in based on the keyword
;				STEP.
;	STEP
;		Scalar, specifies the magnitude of energy step when 2-element ERANGE is
;		used.  Default value is 0.1 keV.
;	FIXED
;		Character string, specifies evaluation mode.
;		The DCD geometry is characterized by three lengths:
;			1)	Full Length: distance from first crystal to sample center.
;			2)	Longitudinal offset of second crystal from first.
;			3)	Transverse offset, of second crystal from first.
;		At any given energy, fixing one of these determines the values of the
;		other two.  Accordingly, FIXED accepts one of three possible inputs:
;		'FULL', 'LONGITUDINAL', TRANSVERSE'.  Case doesn't matter and only
;		first three characters matter.
;	LENGTH
;		Numeric scalar, the length of the item specified by FIXED, in meters.
;	CRYSTAL
;		Character scalar, specifies the type of crystal used for both
;		reflections.  Currently three options are available, 'ge', 'si' and 'di'
;		(standing for diamond).  Default option is 'ge'.
;		The reflections used are always [111] for first crystal and [220] for
;		the second.
;	/RADIANS
;		Switch.  If set, the FTET and STET returns (see below) are in radian
;		units.  Defauls is degrees.
;	/PRECISE
;		Switch.  Controls the number of significant digits in display, in Mode
;		1 (changing from 4 to 6 digits after the decimal point).  Has no effect
;		in Mode 2.
;	/NO_SHOW
;		Switch.  If set, no screen display is done, calculated values are still
;		returned through the optional outputs.
;	ENE
;		Optional output, see below.
;	FTET
;		Optional output, see below.
;	STET
;		Optional output, see below.
;	FUL
;		Optional output, see below.
;	LON
;		Optional output, see below.
;	TRA
;		Optional output, see below.
;	_EXTRA
;		A formal keyword used to pass keywords to imbedded routines.  Not to be
;		used directly.
; OUTPUTS:
; 		Mode 1 (single energy value) :	The values of Full Length,
; 		Longitudinal Offset and Transverse Offset are printed to the screen.
; 		Mode 2 (energy range) :			Plots of the two parameters that are
; 		changing are displayed to the screen.
; OPTIONAL OUTPUT PARAMETERS:
;	ENE
;		Returns the energy value(s) used in the evaluation.
;	FTET
;		Returns the first crystal angle value(s) used in the evaluation, in
;		degrees, unless /RADIANS is set.
;	STET
;		Returns the first crystal angle value(s) used in the evaluation, in
;		degrees, unless /RADIANS is set.
;	FUL
;		Returns the Full Length value(s) used in the evaluation.
;	LON
;		Returns the Longitudinal Offset value(s) used in the evaluation.
;	TRA
;		Returns the Transverse Offset value(s) used in the evaluation.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Calculates using the equations from the "Basic DCD Geometry" writeup.
;		Calls BRAGG_ANGLE from MONO_LIB.  Calls DEFAULT, LABELS, MAKE_GRID,
;		PLVAR_KEEP, STRMATCH_MM and WHERINSTRUCT, from MIDL.
; MODIFICATION HISTORY:
;		Created 15-OCT-2020 by Mati Meron.
;		Documented 25-DEC-2020 by Mati Meron.
;-

	on_error, 1

	posib = ['Full','Longitudinal','Transverse']
	if keyword_set(pre) then pform = 'f9.6," m")' else pform = 'f7.4," m")'

	wstp = Default(stp,0.1,/dtyp)
	case n_elements(ern) of
		0	:	message, 'Missing ehergy input!'
		1	:	ene = ern
		2	:	ene = Make_grid([min(ern,max=max),max],wstp,/step)
		else:	message, 'ERANGE must have 1or 2 elements!'
	endcase
	nene = n_elements(ene)
	mfl = nene gt 1

	crs = Default(crs,'ge',/dtyp)
	ftet = Bragg_angle(ene=ene,cry=crs,ind=[1,1,1],/rad)
	stet = Bragg_angle(ene=ene,cry=crs,ind=[2,2,0],/rad)
	sfac = sin(2*(stet-ftet))/sin(2*stet)

	wlen = Cast(len,4)
	whi = Strmatch_mm(fix,posib,3)
	case whi of
		-1	:	message, 'Missing or invalid mode!'
		0	:	begin
					if mfl then ful = replicate(wlen,nene) else ful = wlen
					lon = ful*sfac*cos(2*ftet)
					tra = ful*sfac*sin(2*ftet)
					if mfl then begin
						fir = lon
						sec = tra
						ftit = 'Longitudinal offset'
						stit = 'Transverse offset'
						gtit = 'Full length = ' + $
						string(ful[0],form='(' + pform)
					endif
				end
		1	:	begin
					if mfl then lon = replicate(wlen,nene) else lon = wlen
					tra = lon*tan(2*ftet)
					ful = lon/(sfac*cos(2*ftet))
					if mfl then begin
						fir = ful
						sec = tra
						ftit = 'Full length'
						stit = 'Transverse offset'
						gtit = 'Longitudinal offset = ' + $
						string(lon[0],form='(' + pform)
					endif
				end
		2	:	begin
					if mfl then tra = replicate(wlen,nene) else tra = wlen
					lon = tra/tan(2*ftet)
					ful = tra/(sfac*sin(2*ftet))
					if mfl then begin
						fir = ful
						sec = lon
						ftit = 'Full length'
						stit = 'Longitudinal offset'
						gtit = 'Transverse offset = ' + $
						string(tra[0],form='(' + pform)
					endif
				end
	endcase

	if not keyword_set(nos) then begin
		if mfl then begin
			bsiz = 64
			dwin = !d.window
			phi = 0.94
			xtit = 'E (keV)'
			ytit = 'Length (m)'
			pcol = !pcol.dred
			xlab = 0.5
			ylab = 0.96

			Plvar_keep, act = 'sav'
			window, dwin + 1, xsi = 8*bsiz, ysi = 12*bsiz
			!p.region = [0,phi/2,1,phi]
			plot, ene, fir, /nodata, /ynoz, xstyle= 1, $
				tit= ftit, xtit= xtit, ytit=ytit, _extra=_e
			oplot, ene, fir, thi=2, col=pcol
			!p.region = [0,0,1,phi/2]
			plot, ene, sec, /nodata, /ynoz, xstyle= 1, /noerase, $
				tit= stit, xtit= xtit, ytit=ytit, _extra=_e
			oplot, ene, sec, thi=2, col=pcol
			dum = Wherinstruct('chars',_e)
			if dum ge 0 then _e.(dum) = 1.2*_e.(dum)
			Labels, xlab,ylab,gtit, align= 0.5, /normal, charsize=1.5, _extra=_e
			Plvar_keep, act = 'res'
			wset, dwin > 0
		endif else begin
			print
			print, ful, form= '("	Full length		= ",' + pform
			print, lon, form= '("	Longitudinal offset	= ",' + pform
			print, tra, form= '("	Transverse offset	= ",' + pform
			print
		endelse
	endif

	if not keyword_set(rad) then begin
		ftet = !radeg*ftet
		stet = !radeg*stet
	endif

	return
end