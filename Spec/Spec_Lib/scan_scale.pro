Function Scan_scale, scan, svar, factor = fact, param = par, power = pow, $
	xscale = xsc, absval = abv, partial = prt, _extra = _e

;+
; NAME:
;		SCAN_SCALE
; VERSION:
;		8.08
; PURPOSE:
;		Multiplies the data (or, optionally, the abscissa) in a scan by a 
;		provided function.
; CATEGORY:
;		SPEC data processing.
; CALLING SEQUENCE:
;		Result = SCAN_SCALE( SCAN [,SVAR] [, keywords])
; INPUTS:
;	SCAN
;		Either a single valid scan, i.e. a [3,n] array, or a single scan number.
;		In the second case the scan will be read from the currently open SPEC
;		file.  Note that in the second case the appropriate data columns (see
;		SCAN_READ) need to be provided.
;
;		Note:	"partial" (2-column) scans are also acceptable, see PARTIAL.
; OPTIONAL INPUT PARAMETERS:
;	SVAR
;		Numeric vector, the scaling variable.  Must be of the same length as
;		the data length, or of length 1, in which case it is replicated to the
;		data length.  If not given, and XSCALE (see below) is not set, the X 
;		variable (i.e. the first column) of SCAN is used as SVAR.
; KEYWORD PARAMETERS:
;	FACTOR
;		Optional factor function.  If given as a character value, the value is
;		taken to represent a function name and the data is multiplied by
;		FACTOR(SVAR).  The data errors are adjusted appropriately.
;	PARAM
;		If given and a function name is provided in FACTOR, PARAM is passed as
;		a parameter to said function.
;	POWER
;		If given as a numeric value, the multiplicative factor (SVAR or a
;		or a function of it) is raised to the power of POWER.
;	/XSCALE
;		Switch.  If set, the abscissa (X variable) of the data is scaled 
;		instead of the data itself.  Note that in this case the optional 
;		substitution of the X variable for the scaling function is disabled.
;	/ABSVAL
;		 Switch.  If set the absolute value of SVAR is used for scaling.
; 	/PARTIAL
; 		Switch.   If set, "partial scans", i.e. [2,n] arrays are accepted.
;	_EXTRA
;		A formal key, used to transfer additional keywords to SCAN_READ.  Only
;		active when a scan number, not explicit scan, is given.
; OUTPUTS:
;		Normally returns an output in a scan format (i.e. a [3,n] array), where
;		the 3 columns (in order) are:
;			0	:	X values, unchanged from the original.
;			1	:	Y values, given by the original ones multiplied by SVAR
;					or a function of it, as specified.
;			2	:	Y error values, given by the original ones multiplied by
;					the absolute value of the factor used in (1).
;
;		When /XSCALE is set, the output is in same format but the X values are
;		scaled while the Y and Y error values remain unchanged.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		If a scan number list is provided, SCAN_READ requires COLUMNS input.
;		See there for details.
; PROCEDURE:
;		Straightforward.  Calls SCAN_LIST_VER and SCAN_READ (only when in the
;		list mode).  Uses CAST, ISNUM, FPU_FIX and TYPE from MIDL.
; MODIFICATION HISTORY:
;		Created 20-FEB-2004 by Mati Meron.
;		Modified 15-APR-2004 by Mati Meron.  Changed last data columns from
;		squares of Y-errors into plain Y-errors.
;		Modified 25-OCT-2008 by Mati Meron.  Added keywords XSCALE and ABSVAL.
;		Modified 15-AUG-2011 by Mati Meron.  Internal changes to accomodate 
;		partial scans.  Added keyword PARTIAL.
;-

	on_error, 1

	nsc = Scan_list_ver(scan,flag=lfl,partial=prt,pflag=pfl,lis=slis)
	if nsc eq 0 then message, 'Missing or inconsistent input!' $
	else if nsc gt 1 then message, 'Only a single scan allowed!'

	if lfl then res = Scan_read(scan,_extra=_e) else res = scan
	len = (size(res))[2]

	xsfl = keyword_set(xsc)
	if Isnum(svar) then tvar = svar $
	else if xsfl then message, 'Missing Scaling input!' $
	else tvar = res[0,*]
	if keyword_set(abv) then tvar = abs(tvar)

	if n_elements(tvar) eq 1 then tvar = Cast(replicate(tvar,len),4) $
	else if n_elements(tvar) ne len then message, 'SVAR size mismatch!'
	tvar = reform(tvar)

	if Type(fact) ne 7 then mfac = tvar $
	else if n_elements(par) eq 0 then mfac = call_function(fact,tvar) $
	else mfac = call_function(fact,tvar,par)
	if Isnum(pow) then mfac = mfac^pow

	if xsfl then res[0,*] = mfac*res[0,*] $
	else if pfl then res[1,*] = mfac*res[1,*] $
	else res[1:2,*] = res[1:2,*]*transpose([[mfac],[abs(mfac)]])

	return, FPU_fix(res)
end