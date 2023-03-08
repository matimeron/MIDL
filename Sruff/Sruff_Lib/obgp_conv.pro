Function OBGP_conv, field = bfl, gap = gap, period = per, scu = scu, mask= msk,$
	_extra = _e

;+
; NAME:
;		OBGP_CONV
; VERSION:
;		8.44
; PURPOSE:
;		Given two of the values field, gap, period, calculates the third.
; CATEGORY:
;		SR specific
; CALLING SEQUENCE:
;		Result = OBGP_CONV, keywords
; INPUTS:
;		None
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	FIELD										|	
;		The magnetic field, in Tesla.			|	Two and only two of these
;	GAP											|	parameters must be provided
;		ID gap, in mm.							|	at any given time (unless
;	PERIOD										|	MASK is used, see below
;		ID period, in mm.						|	
;
;	Note:	The inputs must be either scalars, scalar and array, or two arrays
;			of same size.  
;	MASK
;		An optional mask which can be used to force OBGP_CONV to ignore some
;		of the inputs.  Provided as numerical array, any 0 value translates to
;		"ignore" (and any nonzero value to "accept").  See the routine HOW_MANY
;		in MIDL_LIB for details.
;
;		Alternatively, the mask can also be provided as a character array,
;		specifying explicitly the two inputs to be used, out of the list
;		['FIELD','GAP','PERIOD'].  Only the first 2 characters are needed and
;		case or order do not matter.
;
;		Note:	When MASK is used, one can have more than 2 defined inputs.
;				The number of inputs which are both "defined" and "accepted"
;				(i.e. corresponding to nonzero MASK entries) must still be 2.
;	/SCU
;		Switch.  Specifies that superconducting (NbTi) magnets are used.
;	_EXTRA
;		Formal keyword used to pass keyword to embedded routines.  Not to be
;		used directly.
; OUTPUTS:
;		For whatever two out of the three input parameters are provided, returns
;		the value(s) of the third one.
; OPTIONAL OUTPUT PARAMETERS:
; 		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		As mentioned above, when one of the inputs is a scalar and the other an
;		arrray, the scalar is reformated as an array.  Also, on return all
;		parameters are of type no lower than 4 (float).
; RESTRICTIONS:
;		The calculations of field values have a restricted range (see OID_FIELD
;		for details).  For out of range inputs, Nan will be returned.
; PROCEDURE:
;		Follows the magnetic field relationships, see OID_FIELD for details.
;		Uses OBGP_CONV_FUN (for internal evaluations), OID_FIELD and OSCU_FIELD.
;		Calls CALCTYPE, CAST, HOW_MANY, ISNUM, ROOT, STRMATCH_MM, TOLER and 
;		TYPE, from MIDL.
; MODIFICATION HISTORY:
;		Created 25-OCT-2015 by Mati Meron, based on ID_CONV.
;		Obsoleted and renamed OBGP_CONV 15-JUL-2020 by Mati Meron.
;-

	on_error, 1
	posib = ['field','gap','period']

	typ = Calctype(0.,bfl,gap,per,def=4)
	if n_elements(mask) ne 0 then begin
		if Type(msk) eq 7 then begin
			wmsk = intarr(n_elements(posib))
			for i = 0, n_elements(msk) - 1 do begin
				j = Strmatch_mm(msk[i],posib,2)
				if j ge 0 then wmsk[j] = 1
			endfor
		endif else if Isnum(msk) then wmsk = msk
		if n_elements(wmsk) ne 3 then message, 'Mask needs 3 elements!'
	endif

	check = [n_elements(bfl),n_elements(gap),n_elements(per)]
	if Isnum(wmsk) then check = check*wmsk
	cg0 = check gt 0
	if cg0[0] then wbfl = Cast(bfl,typ)
	if cg0[1] then wgap = Cast(gap,typ)
	if cg0[2] then wper = Cast(per,typ)
	dum = where(check gt 1,ndum)
	if ndum ge 1 then begin
		ndat = max(check[dum],min=mdat)
		if mdat eq ndat then begin
			ce1 = check eq 1
			if ce1[0] then wbfl = replicate(wbfl,ndat)
			if ce1[1] then wgap = replicate(wgap,ndat)
			if ce1[2] then wper = replicate(wper,ngap)
		endif else message, 'All vector inputs must be of same size!' 
	endif else ndat = 1

	wha = How_many(fir=bfl,sec=gap,thi=per,mask=wmsk,whi=whi)
	if keyword_set(scu) then begin	
		res = make_array(ndat,typ=typ)
		inf = machar(double=(typ ge 5))
		kap = -inf.minexp*alog(2)/!pi
		case whi of
			3	:	begin
						ran = [(min(wgap)/kap) > Toler(),1/Toler()]
						for i = 0, ndat-1 do begin
							res[i] = Root('obgp_conv_fun',$
							ran,par=[3,wbfl[i],wgap[i]],sta=sta)
							if not sta then res[i] = !values.f_nan
						endfor
					end
			5	:	begin
						ran = [Toler(),kap*max(wper)]
						for i = 0, ndat-1 do begin
							res[i] = Root('obgp_conv_fun',$
							ran,par=[5,wbfl[i],wper[i]],sta=sta)
							if not sta then res[i] = !values.f_nan
						endfor
					end
			6	:	begin
						wrat = wgap/wper
						for i = 0, ndat-1 do res[i] = $
						OSCU_field(wrat[i],wgap[i])
					end
			else:	message, '2 and only 2 inputs need to be provided!'
		endcase
	endif else begin
		case whi of
			3	:	res = wgap/OID_field(wbfl,/inv,/qui,_extra=_e)
			5	:	res = wper*OID_field(wbfl,/inv,/qui,_extra=_e)
			6	:	res = OID_field(wgap/wper,/qui,_extra=_e)
			else:	message, '2 and only 2 inputs need to be provided!'
		endcase
	endelse

	if ndat eq 1 then res = res[0]
	return, Cast(res,typ,typ,/fix)
end