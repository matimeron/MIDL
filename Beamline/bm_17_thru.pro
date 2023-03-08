Pro BM_17_thru, e, tab = tab, _extra = _e

;+
; NAME:
;		BM_15_THRU
; VERSION:
;		5.0
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
;			3)	DEFAULT, STREQ, STRMATCH_MM and TABULATE from MIDL.
; MODIFICATION HISTORY:
;		Created 10-JAN-2005 by Mati Meron.
;-

	on_error, 1

	tcol = [!pcol.red, !pcol.green, !pcol.blue, !pcol.purple, !pcol.cyan]
	pcol = [2,5,8,11,12]
	if Streq(!d.name,'win') then col = tcol else col = pcol

	filter = 'Be'
	filth = 4*0.25
	airpath = 100.
	crys = 'Si'
	mirel = 'Pd'
	miran = 3e-3
	mirlen = 1.
	tet = 1.5
	psi = 0.086

	bran = Bragg_angle(ene=e,crys=crys,ind=[1,1,1],dar=dar,eta=eta)
	eta = eta[0]
	ftru = exp(-0.1*filth*Abs_coeff(e,elem=filter))
	atru = exp(-0.1*airpath*$
	Abs_coeff(e,elem=['n','o','ar'],wei=[78.,21,1],/form,dens=29./22400))
	cref = Ref_curve(0,ene=e,crys=crys,ind=[1,1,1])
	mref = Mirror(e,miran,elem=mirel)

	flux = BMG_spec(e,/def,tet=tet,psi=psi,ban=eta)
	fflux = ftru*atru*cref^2*mref^2*flux

;	plotot, e, [[flux],[fflux]], line = [2,0], _extra = _e
	plot, e, fflux, _extra= _e, xtit='Energy (keV)', ytit='Flux (ph/s)',line=2

	sagf = BM_17_mirr(e)
	ffflux = sagf*fflux
	oplot, e, ffflux, _extra = _e

;	text=['White, '+ wcrys[0] + ' bw','Pink, '+wcrys[0] + ' bw', $
;	'Mono Beam', 'On sample, ncol', 'On sample, col']

;	Legend, loc = 'll',wid=0.4,lin=lonarr(5),col=col, text=text, charsize=csiz

	if keyword_set(tab) then Tabulate, e, fflux, $
	head = ['Energy (keV)','Flux (ph/s)'], tit='17BM Total Flux'

	return
end