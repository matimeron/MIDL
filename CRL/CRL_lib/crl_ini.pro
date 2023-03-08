Function CRL_ini, station = stn, lenmax = lmx, warn = wrn, $
	sorloc = sol, optloc = opl, samloc = sal,  mirloc = mil, slope_err = sle, $
	elem = ele, radius = rad, aperture = apr, thick = thi, rough = rof, $
	selem= sele, sradius= srad, saperture= sapr, sthick= sthi, srough= srof

;+
; NAME:
;		CRL_INI
; VERSION:
;		8.73
; PURPOSE:
;		Generates a structure containing information for CRL evaluations.
; CATEGORY:
;		Optics, ChemMat Specific.
; CALLING SEQUENCE:
;		Result = CRL_INI( STATION = STN [, keywords])
; INPUTS:
;		None.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 	STATION
; 		Character input (only first letter matters), identifying experimental
; 		station.  Acceptable values are 'C' and 'D' for 15-ID1 and 'E' and 'B'
; 		for 15-ID2.  Invoking any of these options fills the structure with the
; 		appropriate values for said station.
;
; 		Note:	Some values may depend on whether 'APS' or 'APSU' is assumed.
; 				See system variable !BLPAR for details.
; 	LENMAX
; 		Integer scalar, specifies maximal number of lenses.
;	SORLOC
;		The source distance from the reference point (center of straight
;		section).  Default values are:
;			15-ID1:	1.25m for 'APS", -1.25m for 'APSU'
;			15-ID2:	1.25m
;	OPTLOC
;		The optics (lens) distance from the reference point, in m.  Default
;		values are:
;			15-ID1:	46.7m
;			15-ID2:	34.3m
;	SAMLOC
;		Same as OPTLOC, for the sample.  Default values are:
;			15-ID1:	56.0m for station C, 61.5m for station D
;			15-ID2:	42.9m for station E, 51.2m for station B.
;	MIRLOC
;		Location of the reflecting mirror(s) from the reference point, in m.
;		Used only when SLOPE_ERR (see below) is provided).  Default is 32.5m.
;		The value is always forced to be <= OPTLOC.
;		Alternatively, a negative value may be given, and it is interpreted as
;		the distance from the sample location.  In this case the absolute value
;		is forced to be <= (SAMLOC - OPTLOC).
;		No defaults.
;	SLOPE_ERR
;		The RMS value of the mirror slope error, in microradians.  Default is 0.
;		If provided and non-zero, the broadening effect of slope errors is added
;		to the evaluation.
;	ELEM
;		The physical element from which the lens is made.  Default values are:
;			15-ID1:	Al
;			15-ID2: Be
;	RADIUS
;		The radius of curvature at the tip of the lens parabola, in mm.  Default
;		values are:
;			15-ID1:	0.2mm
;			15-ID2:	0.3mm
;	APERTURE
;		The lens aperture, in mm.  Default values established internally from
;		the RADIUS value.
;	THICK
;		The minimal (center) thickness of the lens, in mm.  Default values are
;			15-ID1: 0.02mm (Al value)
;			15-ID2: 0.1mm (Be value)
;	ROUGHNESS
;		The lens roughness, in Angstrem.  Default is 3000 Angstrem.
;	SELEM
;		The physical element from which second type of lens is made, only if
;		such lens is used.  Default values are
;			15-ID1:	None.
;			15-ID2: Be
;	SRADIUS
;		The radius of curvature at the tip of the second lens parabola, in mm.
;		Default values are:
;			15-ID1:	None
;			15-ID2:	1mm
;	SAPERTURE
;		The second lens aperture, in mm.  If second lens exists, default values
;		established internally from the SRADIUS value.
;	STHICK
;		The minimal (center) thickness of the second lens, in mm.  Default
;		values are
;			15-ID1: None
;			15-ID2: 0.1mm (Be value)
;	SROUGHNESS
;		The second lens roughness, in Angstrem.  Default is 3000 Angstrem, same
;		as for first lens.
; OUTPUTS:
;		Returns a structure of {CRL_PARS} type, with the required fields filled
;		out.  See CRL_PARS__DEFINE for details.
; OPTIONAL OUTPUT PARAMETERS:
; 		None.
; COMMON BLOCKS:
;		CRL_INFO.  Contains:
;			EXS		-	Status variable, 1 if structure exists, 0 otherwise.
;			CRLDAT	-	Last defined structure.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Defines if needed the structure, calling CRL_PARS_DEFINE, and fills it
;		up based on the input keywords.
;		Calls CHECK_STRUCT, HOW_MANY, ISNUM, STREQ and STRMATCH_MM, from MIDL.
; MODIFICATION HISTORY:
;		Created 25-JAN-2022 by Mati Meron.
;-

	common CRL_info, exs, crldat
	on_error, 1

	if not Isnum(exs) then crldat = {crl_pars} $
	else if not(exs) then crldat = {crl_pars}
	exs = 1

	posib = ['C','D','E','B','X']
	if keyword_set(stn) then begin
		istn = Strmatch_mm(stn,posib,1,/nosub)
		if istn ge 0 then begin
			crldat.station = posib[istn]
			case istn/2 of
				0	:	begin
							check = Streq(!blpar.sync,'aps',4)
							if check then crldat.sol = 1.25 $
							else crldat.sol= -1.25
							crldat.opl = 46.7
							if istn eq 0 then crldat.sal = 56.0 $
							else crldat.sal = 61.5
							crldat.lenset = 1
							crldat.lenmax = 95
							crldat.len.ele = 'Al'
							crldat.len.dfc = 1.
							crldat.len.rad = 0.2
							crldat.len.apr = 2*sqrt(crldat.len.rad)
							crldat.len.thi = 0.02
							crldat.len.rof = 3000.
							crldat.xlen = {rlens}
						end
				1	:	begin
							crldat.sol = 1.25
							crldat.opl = 34.3
							if istn eq 2 then crldat.sal = 42.9 $
							else crldat.sal = 51.2
							crldat.lenset = 2
							crldat.lenmax = 63
							crldat.len.ele = 'Be'
							crldat.len.dfc = 1.
							crldat.len.rad = 0.3
							crldat.len.apr = 2*sqrt(crldat.len.rad)
							crldat.len.thi = 0.1
							crldat.len.rof = 3000.
							crldat.xlen = crldat.len
							crldat.xlen.rad = 1.
							crldat.xlen.apr = 2*sqrt(crldat.xlen.rad)
						end
				2	:	begin
							crldat = {crl_pars}
							crldat.station = posib[istn]
						end
			endcase
		endif else message, 'Unknown station!'
	endif

	whaf = $
	How_many(fir=sol,sec=opl,thi=sal,fou=mil,fif=sle,six=lmx,whi=cod,/alt)
	if whaf gt 0 then begin
		if cod[0] then crldat.sol = sol
		if cod[1] then crldat.opl = opl
		if cod[2] then crldat.sal = sal
		if cod[3] and cod[4] then begin
			if sle gt 0 then begin
				if mil gt 0 then begin
					crldat.mil = mil < crldat.opl
					crldat.mifl = 1
				endif else begin
					crldat.mil = (crldat.sal + mil) > crldat.opl
					crldat.mifl = -1
				endelse
			endif else crldat.mifl = 0
		endif else crldat.mifl = 0
		if cod[5] then crldat.lenmax = lmx
	endif

	whas = How_many(fir=ele,sec=rad,thi=apr,fou=thi,fif=rof,whi=cod,/alt)
	if whas gt 0 then begin
		crldat.lenset = crldat.lenset > 1
		if cod[0] then crldat.len.ele = ele
		if Streq(crldat.len.ele,'C') then crldat.len.dfc = 2.068 $
		else crldat.len.dfc = 1.
		if cod[1] then crldat.len.rad = rad
		if cod[2] then crldat.len.apr = apr $
		else crldat.len.apr =  2*sqrt(crldat.len.rad)
		if cod[3] then crldat.len.thi = thi
		if cod[4] then crldat.len.rof = rof
	endif

	what = How_many(fir=sele,sec=srad,thi=sapr,fou=sthi,fif=srof,whi=cod,/alt)
	if what gt 0 then begin
		crldat.lenset = crldat.lenset > 2
		if cod[0] then crldat.xlen.ele = sele
		if Streq(crldat.xlen.ele,'C') then crldat.xlen.dfc = 2.068 $
		else crldat.xlen.dfc = 1.
		if cod[1] then crldat.xlen.rad = srad
		if cod[2] then crldat.xlen.apr = sapr $
		else crldat.xlen.apr =  2*sqrt(crldat.xlen.rad)
		if cod[3] then crldat.xlen.thi = sthi
		if cod[4] then crldat.xlen.rof = srof
	endif

	if keyword_set(wrn) then begin
		if crldat.lenmax eq 0 then crldat.lenmax = 1023
		fir = Check_struct(crldat,['opl','sal','lenset'])
		sec = Check_struct(crldat.len)
		if crldat.lenset eq 2 then thi = Check_struct(crldat.xlen) else thi = 1
		if not (fir*sec*thi) then message, 'Missing data, check output',/con
	endif

	return, crldat
end