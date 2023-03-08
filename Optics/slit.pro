Pro Slit, wid, lambda= lam, asize= asz, colen= csz, hsize= hsz, tet_lim= tlm, $
	qscale = qsc, show_initial = shi, hi_prec = hpr, fwhm = fwhm, swid = swid,$
	transmission = trn, _extra = _e

;+
; NAME:
;		SLIT
; VERSION:
;		4.3
; PURPOSE:
;		Calculates the difraction of a realistic beam through a slit.
; CATEGORY:
;		Optics calculation.
; CALLING SEQUENCE:
;		Slit [keywords]
; INPUTS:
;	WID
;		Slit width, in wavelength units (unless LAMBDA is defined).
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	LAMBDA
;		Wavelength.  Units arbitrary but must be same as the units of WID,
;		COLEN and HSIZE.
;	ASIZE
;		Input beam angular size, in radians.
;	COLEN
;		Input beam coherence length, in wavelength units (unless LAMBDA is
;		defined).
;	HSIZE
;		Input beam full spatial size, in wavelength units (unless LAMBDA is
;		defined).
;	TET_LIM
;		The angular range over which display is required.  By default slit sets
;		the range internally based on the values of relevant parameters.
;	/QSCALE
;		Switch.  If set, the results are desplayed as a function of
;		q*w = k*wid*theta = 2*pi*wid*theta/lambda.  The default is to display
;		the results as a function of the angle theta.
;	/SHOW_INITIAL
;		Switch.  If set, both the initial and final distributions are displayed.
;	/HI_PREC
;		Switch.  Increases the integration resolution (by order of magnitude).
;		Improves accuracy for large scattering angles (paying with speed).
;	/FWHM														|
;		Switch.  If set, the fwhm value for the diffracted 		|	Only one of
;		(and initial if /SHOW_INITIAL is set) beam is displayed.|	these two
;	/SWID														|	may be used
;		Switch.  If set, the "square-width" value for the 		|	at any time
;		diffracted (and initial if /SHOW_INITIAL is set) beam 	|
;		is displayed.  The input sizes of used ASIZE, COLEN and
;		HSIZE are understood to be "square-widths".
;	/TRANSMISSION
;		Switch.  If set, the fraction of the original beam transmitted through
;		the slit is displayed.
;	_EXTRA
;		A formal keyword used to transfer keywords to plotting routines.  Not
;		to be used directly.
; OUTPUTS:
;		None other than graphic display.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The inputs must satisfy LAMBDA/ASIZE < COLEN < HSIZE.
; PROCEDURE:
;		Complex integration based on GWO.  Details elsewhere.  Calls SLITINT.
;		Calls PEAK_FWHM and PEAK_SWID from SPEC.  Calls CENTFT, DEFAULT,
;		FPU_FIX, HOW_MANY, ISNUM, MAKE_GRID, PLOTOT, PREP_FT, REAL_MM, TOLER
;		and WHERINSTRUCT, from MIDL.
; MODIFICATION HISTORY:
;		Created 15-APR-2003 by Mati Meron.
;-

	on_error, 1

	pmult = 10
	q = 5
	hpmult = 10

	wlam = Default(lam,1.,lo=4)
	wwid = wid/wlam
	eps = Toler()/wwid
	wasz = Default(asz,eps,lo=4)
	wcsz = Default(csz,wlam/eps,lo=4)/wlam
	whsz = Default(hsz,wlam/eps,lo=4)/wlam
	if whsz lt wcsz or wcsz lt 1/wasz then message, 'Unacceptable sizes'

	sigc = wcsz/wwid/sqrt(!pi)
	sigh = whsz/wwid/sqrt(!pi)
	zeta = whsz/wcsz*sqrt((wasz*wcsz)^2 - 1)

	if keyword_set(qsc) then qfac = 2*!pi*wwid else qfac = 1.
	if keyword_set(hpr) then pmult = pmult*hpmult
	p = pmult*ceil(max([1,1/sigc,zeta/sigh,zeta/sigh^2]))
	n = Prep_ft(2*p*q + 1)
	t = Make_grid((n-1.)/(2*p)*[0,1],(n+1)/2)
	slint = Slitint(t,sigma=sigh,zeta=zeta)
	dist = Centft(t,exp(-t^2/(2*sigc^2))*slint,/sym,coo=psi,/int)
	dist = FPU_fix(sqrt(2/!pi)*wwid/(sigh*qfac)*Real_mm(dist))
	tet = psi/(2*!pi*wwid)

	shifl = keyword_set(shi)
	if shifl then begin
		oamp = sqrt(2)/(wasz*qfac)
		odist = FPU_fix(oamp*exp(-2*!pi*(tet/wasz)^2))
		if (Wherinstruct('ylog',_e))[0] ge 0 then begin
			lplt = 1
			yrn = [Toler()/oamp,oamp]
		endif else lplt = 0
	endif

	tet = qfac*tet
	if How_many(fir=fwhm,sec=swid,thi=trn,whi=whi) gt 0 then begin
		print
		if whi and 1 then begin
			print, 'FWHM, transmitted 	= ', Peak_fwhm(tet,dist)
			if shifl then $
			print, 'FWHM, incoming		= ', Peak_fwhm(tet,odist)
			print
		endif
		if (whi and 2)/2 then begin
			print, 'SWID, transmitted 	= ', Peak_swid(tet,dist)
			if shifl then $
			print, 'SWID, incoming		= ', Peak_swid(tet,odist)
			print
		endif
		if (whi and 4)/4 then begin
			print, 'Transmitted fraction	= ',Integ(tet,dist,/val)
			print
		endif
	endif

	if Isnum(tlm) then begin
		dum = where(abs(tet) le tlm, ndum)
		if ndum gt 0 then begin
			tet = tet[dum]
			dist = dist[dum]
			if shifl then odist = odist[dum]
		endif else message, 'No data in range!'
	endif

	if shifl then begin
		if lplt then Plotot,tet,[[dist],[odist]],yran=yrn,line=[0,2],_extra=_e $
		else Plotot,tet,[[dist],[odist]],line=[0,2],_extra=_e
	endif else plot, tet, dist, _extra = _e

	return
end