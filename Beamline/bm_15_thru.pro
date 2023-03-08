Pro BM_15_thru, e, chi, crys = crys, tab = tab, charsize = csiz

;+
; NAME:
;		BM_15_THRU
; VERSION:
;		8.0
; PURPOSE:
;		Throughput calculations for the proposed BM_15 beamline.
; CATEGORY:
;		CARS specific.
; CALLING SEQUENCE:
;		BM_15_thru, E [,CHI],
; INPUTS:
;	E
;		Energy, in keV.  Scalar or vector.
;	CHI
;		The steering angle Chi, in Degrees.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	CRYS
;		Character scalar or two element vector.  Specifies the DCM and steering
;		crystals.  Possible options are 'Di' (diamond), 'Si', 'Ge' and 'ML'
;		(multilayer).  If a single entry is given, it is used both for the DCM
;		and the steering crystal.
;	/TAB
;		Switch.  If set, a table of values is printed to the screen.
;	CHARSIZE
;		Specifies the character size for plotting,  Default is 1.
; OUTPUTS:
;		None, other than screen output.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The energy values in E must be above the minimum attainable by the
;		monochromator (about 3 keV for diamond, 2 keV for Si, Ge, 0.3 keV for
;		multilayer
; PROCEDURE:
;		Evaluates flux based on the bending magnet spectral function, and the
;		reflectivities and bandwidths of the mirrors and crystals.  Calculation
;		assumes 2 Pt mirrors, a vertically reflecting DCM and a steering crystal
;		at orientation specified by Chi.
;
;		The procedure generates and graphically displays the following sets of
;		values:
;
;		a)	White beam flux within the DCM bandwidth.
;		b)	Same as (a) for the pink beam.  This is reduced from (a) by the
;			reflectivities of the mirrors.
;		c)	Same as (b), after the DCM.  Should be numerically identical to (b)
;			for perfectly reflecting crystal, but the reflectivity of the
;			crystals is never 100%.
;		d)	The flux delivered after the steering crystal, assuming the mirrors
;			are in a non-colimating configuration.
;		e)	Same as (d) with mirrors in a collimating configuration.
;
;		If the keywword /TAB is set, a table of same number sets is printed to
;		the screen.
;
;		Bm_15_Thru calls:
;
;			1)	BMG_SPEC and MIRROR from SRUFF.
;			2)	BRAGG_ANGLE, MONO_OVER and REF_CURVE from MONO.
;			3)	DEFAULT, LEGEND_MM, STREQ, STRMATCH_MM and TABULATE from MIDL.
; MODIFICATION HISTORY:
;		Created 10-JAN-2005 by Mati Meron.
;		Modified 25-AUG-2010 by Mati Meron.  Replaced LEGEND with LEGEND_MM.
;-

	on_error, 1

	tcol = [!pcol.red, !pcol.green, !pcol.blue, !pcol.purple, !pcol.cyan]
	pcol = [2,5,8,11,12]
	if Streq(!d.name,'win') then col = tcol else col = pcol
	posib = ['Di','Si','Ge','ML']

	ering = 7
	bfield = 0.599
	cur = .100

	mirel = 'Pt'
	miran = 2.5e-3
	mirlen = 1.
	dist=30.
	tet = 1
	psi = 1e3*miran*mirlen/dist

	ml2d = 40
	mlref = 0.5
	mleta = 5e-3

	wchi = Default(chi,0.)
	case n_elements(crys) of
		0	:	message, 'What crystal?'
		1	:	wcrys = [crys,crys]
		else:	wcrys = crys[0:1]
	endcase
	for i = 0, 1 do begin
		dum = wcrys[i]
		whi = Strmatch_mm(dum,posib)
		if whi ge 0 then wcrys[i] = dum else message, 'Unknown crystal!'
	endfor

	feta = (seta = mleta)
	fbran = (sbran = asin(!srcon.conv/(e*ml2d)))
	ftan = (stan = tan(sbran))
	fdar = (sdar = seta*stan)
	fref = (sref = mlref)

	if not Streq(wcrys[0],'ml',2) then begin
		fbran = $
		Bragg_angle(ene=e,crys=wcrys[0],ind=[1,1,1],dar=fdar,eta=feta,/rad,/unc)
		feta = feta[0]
		ftan = tan(fbran)
		fref = Ref_curve(0,ene=e,crys=wcrys[0],ind=[1,1,1],/rad)
	endif

	if not Streq(wcrys[1],'ml',2) then begin
		sbran = $
		Bragg_angle(ene=e,crys=wcrys[1],ind=[1,1,1],dar=sdar,eta=seta,/rad,/unc)
		seta = seta[0]
		stan = tan(sbran)
		sref = Ref_curve(0,ene=e,crys=wcrys[1],ind=[1,1,1],/rad)
	endif

	fluxw = $
	BMG_spec(e,eri=ering,bfie=bfield,cur=cur,tet=tet,psi=psi,ban=feta,vsig=sigy)
	lmax = ceil(alog10(max(fluxw)))
	yran = 10.^([lmax-4,lmax])
	tit = 'Flux, chi = ' + string(wchi,form='(f6.2)') +' deg. ; ' + $
	wcrys[0] + ' DCM, ' + wcrys[1] + ' steering'
	plot, e, fluxw, /ylo, yran= yran, tit= strjoin(tit,'!c'), $
	xtit= 'E(keV)', ytit= 'phot/sec', /nodata, charsize=csiz
	oplot, e, fluxw, col = col[0]

	fluxp = fluxw*Mirror(e,miran,elem=mirel)^2
	oplot, e, fluxp, col=col[1]
	fluxm = fref^2*fluxp
	oplot, e, fluxm, col=col[2]

	wchi = !dtor*wchi
	sigx = 1e-3*tet/sqrt(4*!pi)
	sigy = 1e-3*sigy

	xfan = sqrt((sigx*cos(wchi))^2 + (sigy*(stan/ftan - sin(wchi)))^2)/sdar
	xfac = sqrt((sigx*cos(wchi))^2 + (sigy*sin(wchi))^2)/sdar

	fluxs = sref*fluxm
	fluxsn = fluxs*Mono_over(feta/seta,xfan)
	fluxsc = fluxs*Mono_over(feta/seta,xfac)
	oplot, e, fluxsn, col=col[3]
	oplot, e, fluxsc, col=col[4]

	text=['White, '+ wcrys[0] + ' bw','Pink, '+wcrys[0] + ' bw', $
	'Mono Beam', 'On sample, ncol', 'On sample, col']

	Legend_mm, loc = 'll',wid=0.4,lin=lonarr(5),col=col, text=text, charsize=csiz

	if keyword_set(tab) then Tabulate, e, fluxw, fluxp, fluxm, fluxsn, fluxsc, $
	form = ['f7.3','5g11.4'], tit = tit, head = ['E (keV)',text]

	return
end