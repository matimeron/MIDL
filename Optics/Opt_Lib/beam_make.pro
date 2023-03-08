Function Beam_make, beam, element = ele, location = loc, parameter = par, $
	code = cod, elset = els, elname = enm, name = nam, comment = cmm, $
	foc_loc = fcl, foc_type = fct, dif_ang = dan, sigma = sig, internal = int

;+
; NAME:
;		BEAM_MAKE
; VERSION:
;		5.6
; PURPOSE:
;		Fills up a beam structure of type OPBEAM.
; CATEGORY:
;		Optical calculations.
; CALLING SEQUENCE:
;		Result = BEAM_MAKE( BEAM, keywords)
; INPUTS:
;	BEAM
;		Structure of type OPBEAM.  See OPBEAM__DEFINE for details.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	ELEMENT
;		Character vector, a list of optical elements. Mandatory.  Currently
;		accepted elements are:
;			NUL - Null element.
;			FOC - Focusing device.
;			DIF - Diffuser.
;			SSL - Spatial slit (aperture).
;			ASL - Angular slit (aperture).
;
;		Note:	If the letter 'x' (or 'y') is appended as the *last* letter
;		of the element name, said alement is active only along the X (or Y)
;		direction.
;	LOCATION
;		Numeric vector, locations of the optical elements along the beam path,
;		in meters.  Mandatory.
;	PARAMETER
;		Numeric vector, list of parameters pertaining to the optical elements
;		(one parameter per element).  Mandatory.
;	CODE
;		An optional integer vector, the optical codes for the elements (see
;		BEAMSEC__DEFINE for more detail).
;	ELSET
;		An optional integer vector, used to turn on or off specified optical
;		elements.  If provided, elements corresponding to 0 entries in ELSET are
;		disabled (see BEAMSEC__DEFINE for more detail).
;	ELNAME
;		An optional string vector.  If provided, the entries are used as names
;		of the optical elements.
;	NAME
;		Character input, optional, the name of the beam.
;	COMMENT
;		Character input, optional, informational comment.
;	FOC_LOC
;		Optional numeric vector, list of focus locations (for the focusing
;		elements).
;	FOC_TYPE
;		Either integer or character vector, list of codes for focus type.
;		Possible values and corresponding character codes (only first 2 letters
;		matter are:
;			1 ('hsiz')	:	Minimizes beam size at TARGET.  Not a true focus.
;			2 ('colm')	:	Collimation, i.e. focusing to infinity.  TARGET
;							is ignored in this case.
;			3 ('ssiz')	:	Focus with small physical size at TARGET.
;			4 ('asiz')	:	Focus with small angular size at TARGET.
;
;		See the function FOCLN for more detail.  If FOC_LOC is given, FOC_TYPE
;		may be one of the following:
;			a)	Undefined, in which case 'hsiz' is used at all location.
;			b)	Scalar, in which case this entry is used at all locations.
;			c)	Vector of same length as FOC_LOC.
;
;		Note1:	Only elements of type "FOC" for which the focal length (in
;				PARAMETER) is specified as 0 have their focal length evaluated
;				using FOC_LOC and FOC_TYPE.
;		Note2:	The entries of FOC_TYPE can also be provided (as numerical codes
;				through the input CODE.  If provided *both* through CODE and
;				FOV_TYPE, the latter has precedence.
;	DIF_ANG
;		Specifies that some or all of the diffuser parameters are not
;		correlation lengths but slope errors.  In this case the correlation
;		length is given by wavelength/slope_error (divided by sqrt(4*!pi) if
;		SIGMA is set (see below).
;
;		DIF_ANG can be given as a vector, of same length as the number of
;		occurences of "dif" in the element list.  In such case each entry of
;		DIF_ANG applies to one diffuser, with 1 - set and anything else meaning
;		"not_set".  Alternatively it can be given as a scalar to apply to all
;		diffusers.  In this case it can be used as switch.
;
;		Note:	The elements of DIF_ANG play same role as the entries in the
;				(optional) input CODE, corresponding to the diffuser elements.
;				If both CODE and DIF_ANG are given, the latter takes precedence.
;	/SIGMA
;		Switch.  If set, the diffusser parameter(s) only are taken to be
;		sigma-values.  Valid only in conjuction with DIF_ANG, ignored otherwise.
;	/INTERNAL
;		Switch.  Selects between internal (meter, radian) and external
;		(micron, microradian) units.
; OUTPUTS:
;		Returns a structure of type OPBEAM, with the fields filled up according
;		to the input values.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		Block BEAM_STUFF, as defined in INIT_OPT.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		1)  The vectors given by LOCATIONS, ELEMENTS, PARAMETERS and (if
;			provided) ELNAMES and/or MASK must be of same length.
;		2)	The vector FOC_LOC (if provided) must be of same length as the
;			total number of "FOC" type entries in ELEMENTS.
;		3)	The vector FOC_TYPE (if provided and FOC_LOC is provided) must
;			be of length 1 or same as FOC_LOC.
;		4)	The vector DIF_ANG (if provided) must be either of length 1 or same
;			length as the total number of "DIF" type entries in ELEMENTS.
; PROCEDURE:
;		Straightforward.  Calls FOCLN.  Also calls ARREQ, CAST, DEFAULT, ISNUM,
;		STRMATCH_MM, TYPE and VERIFY_STRUCT from MIDL.
; MODIFICATION HISTORY:
;		Created 20-AUG-2001 by Mati Meron as MAKE_BEAM.
;		Modified 30-MAY-2002 by Mati Meron.
;		Modified 5-DEC-2006 by Mati Meron.  Added keywords CODE, DIF_ANG and
;		/SIGMA and changed name to BEAM_MAKE for consistency.
;-

	common beam_stuff, exs, npomax, mmicr, mwlen
	on_error, 1

	posib = ['fpr','nul','foc','dif','ssl','asl']
	fnam = ['fprop','nullf','focus','difus','sslit','aslit']
	if keyword_set(int) then multps = [1.,0.,1.,1.,1.,1.] $
	else multps = [1.,0.,1.,mmicr,mmicr,mmicr]
	if keyword_set(sig) then smult = 1./sqrt(4*!pi) else smult = 1.

	if not Verify_struct(beam,'opbeam') then message, $
	'BEAM must be a scalar structure of type "OPBEAM"!'
	if beam.npoints gt 0 then res = beam else message, 'BEAM not initialized!'

	n = min([n_elements(ele), n_elements(loc), n_elements(par)], max = ndum)
	if n eq 0 or n lt ndum then message, 'Bad or missing input!' $
	else if n gt npomax - 1 then message, 'Excessive number of elements' $
	else res.npoints = n + 1
	inds = 2*(1l + lindgen(n))
	wele = ele
	wloc = Cast(loc,4)
	wpar = Cast(par,4)
	wcod = Default(cod,intarr(n),/dtyp)
	if n_elements(wcod) ne n then message, 'Wrong number of code values!'
	wels = Default(els,replicate(1,n),/dtyp)
	if n_elements(wels) ne n then message, 'Wrong number of element settings!'
	wenm = Default(enm,strarr(n),/dtyp)
	if n_elements(wenm) ne n then message, 'Wrong number of element names!'
	s = sort(loc)
	if not Arreq(loc,loc[s]) then begin
		wele = wele[s]
		wloc = wloc[s]
		wpar = wpar[s]
		wcod = wcod[s]
		wels = wels[s]
		wenm = wenm[s]
	endif
	if Type(nam) eq 7 then res.name = nam
	if Type(cmm) eq 7 then res.comment = cmm
	res.elname[inds] = wenm

	nfl = n_elements(fcl)
	if nfl gt 0 then begin
		finds = where(strpos(wele,posib[2]) eq 0,nfinds)
		if nfinds eq nfl then wfcl = Cast(fcl,4) $
		else message, 'Focusing info size mismatch!'
		case n_elements(fct) of
			0	:	wfct = replicate(1l,nfl)
			1	:	wfct = replicate(fct,nfl)
			nfl	:	wfct = fct
			else:	message, 'Focusing info size mismatch!'
		endcase
		if not Isnum(wfct) then begin
			tfct = wfct
			wfct = intarr(nfl)
			fposib = ['hsiz','colm','ssiz','asiz']
			for i = 0, nfl - 1 do wfct[i] = 1 + Strmatch_mm(tfct[i],fposib,2)
		endif
		dum = where(wpar[finds] eq 0, ndum)
		if ndum gt 0 then begin
			wcod[finds[dum]] = wfct[dum]
			wpar[finds[dum]] = wfcl[dum] - wloc[finds[dum]]
		endif
	endif else begin
		finds = where(strpos(wele,posib[2]) eq 0 and wpar eq 0,nfinds)
		if nfinds gt 0 then message, $
		'Zero focal length with no specified action!'
	endelse

	ndn = n_elements(dan)
	if ndn gt 0 then begin
		dinds = where(strpos(wele,posib[3]) eq 0,ndf)
		case ndn of
			1	:	wdft = replicate(dan,ndf)
			ndf	:	wdft = dan
			else:	message, 'Diffuser info size mismatch!'
		endcase
		wcod[dinds] = wdft
	endif

	lens = strlen(wele)
	check = strpos(wele,'x')
	xon = where(check eq lens-1,nxon)
	if nxon gt 0 then wele[xon] = strmid(wele[xon],0,transpose(check[xon]))
	check = strpos(wele,'y')
	yon = where(check eq lens-1,nyon)
	if nyon gt 0 then wele[yon] = strmid(wele[yon],0,transpose(check[yon]))

	if (res.set and 1) ne 0 then begin
		etem = wele
		ptem = wpar
		ctem = wcod
		if nyon gt 0 then begin
			etem[yon] = posib[1]
			ptem[yon] = 0.
			ctem[yon] = 0
		endif
		res.xsec[inds-1].optel = posib[0]
		res.xsec[inds].optel = etem
		res.xsec[inds-1].zval = wloc
		res.xsec[inds].zval = wloc
		res.xsec[inds-1].optpar = wloc - [0,wloc]
		res.xsec[inds].optpar = ptem
		res.xsec[inds].optcod = ctem
		res.xsec[inds].elset = wels

		for i = 1l, 2*(res.npoints - 1) do begin
			if not i mod 2 then begin
				j = Strmatch_mm(res.xsec[i].optel,posib,3)
				if j lt 0 then message, 'Unknown element!' else j = j > 1
			endif else j = 0
			res.xsec[i].optpar = multps[j]*res.xsec[i].optpar
			if j eq 2 and res.xsec[i].optcod gt 0 then res.xsec[i].optpar = $
			Focln(res.xsec[i-1].bpars,res.xsec[i].optpar,foc=res.xsec[i].optcod)
			if j eq 3 and res.xsec[i].optcod eq 1 then res.xsec[i].optpar = $
			smult*res.wavl/res.xsec[i].optpar
			res.xsec[i].optcod = 0
			if not(res.xsec[i].elset or (j eq 0)) then j = 1
			res.xsec[i].bpars = call_function(fnam[j],res.wavl, $
				res.xsec[i-1].bpars,res.xsec[i].optpar)
		endfor
	endif

	if (res.set and 2) ne 0 then begin
		etem = wele
		ptem = wpar
		ctem = wcod
		if nxon gt 0 then begin
			etem[xon] = posib[1]
			ptem[xon] = 0.
			ctem[xon] = 0
		endif
		res.ysec[inds-1].optel = posib[0]
		res.ysec[inds].optel = etem
		res.ysec[inds-1].zval = wloc
		res.ysec[inds].zval = wloc
		res.ysec[inds-1].optpar = wloc - [0,wloc]
		res.ysec[inds].optpar = ptem
		res.ysec[inds].optcod = ctem
		res.ysec[inds].elset = wels

		for i = 1l, 2*(res.npoints - 1) do begin
			if not i mod 2 then begin
				j = Strmatch_mm(res.ysec[i].optel,posib,3)
				if j lt 0 then message, 'Unknown element!' else j = j > 1
			endif else j = 0
			res.ysec[i].optpar = multps[j]*res.ysec[i].optpar
			if j eq 2 and res.ysec[i].optcod gt 0 then res.ysec[i].optpar = $
			Focln(res.ysec[i-1].bpars,res.ysec[i].optpar,foc=res.ysec[i].optcod)
			if j eq 3 and res.ysec[i].optcod eq 1 then res.ysec[i].optpar = $
			smult*res.wavl/res.ysec[i].optpar
			res.ysec[i].optcod = 0
			if not(res.ysec[i].elset or (j eq 0)) then j = 1
			res.ysec[i].bpars = call_function(fnam[j],res.wavl, $
								res.ysec[i-1].bpars,res.ysec[i].optpar)
		endfor
	endif

	return, res
end