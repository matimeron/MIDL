Function ABC_int, inp, full = ful, radial = rad, angular = ang

;+
; NAME:
;		ABC_INT
; VERSION:
;		8.714
; PURPOSE:
;		Integrates an ABC-type distribution.
; CATEGORY:
;		Optics ABC-formalism calculations.
; CALLING SEQUENCE:
;		Result = ABC_INT ( INP, [, keywords])
; INPUTS:
;	INP
;		An {ABC} or {IABC} type structure.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 	/FULL										|
; 		Switch.  Specifies full integration.	|	No more than one of these
; 	/RADIAL										|	keywords may be set at any
; 		Switch.  Specifies radial integration.	|	given time.  If none is set,
; 	/ANGULAR									|	the default is /FULL
; 		Switch.  Specifies angular integration.	|
;
;	Note:	Integration on energy is automatic, in all cases.  Also, if INP
;			is and {IABC} structure, only FULL integration is available and
;			any other keyword will be ignored.
; OUTPUTS:
;		Returns an ABC structure, populated with values derived from RSIG, ASIG,
;		BANDWIDTH and AMPLITUDE, or their defaults.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Following the integration procedures described in the "ABC-Beam
;		Calculations" write-up.  Calls ONE_OF, STREQ and SVD_INVERT, from MIDL.
; MODIFICATION HISTORY:
;		Created 15-JUNE-2017 by Mati Meron.
;		Documented 5-JUL-2019 by Mati Meron.
;-

	on_error, 1

	if Streq(tag_names(inp,/str),'abc') then begin
		wcon2 = inp.con2 > 2*!dpi
		wha = One_of(ful,rad,ang,/noz) > 0
		if wha gt 0 then begin
			aaahat = inp.amat - (transpose(inp.bvc1)##inp.bvc1)/wcon2
			arrhat = aaahat[0:1,0:1]
			arshat = aaahat[2:3,0:1]
			asrhat = aaahat[0:1,2:3]
			asshat = aaahat[2:3,2:3]
			arrhinv = SVD_invert(arrhat,/ref)
			asshinv = SVD_invert(asshat,/ref)
			bvr0 = inp.bvc0[0:1]
			bvs0 = inp.bvc0[2:3]

			res = {iabc}
			case wha of
				1	:	begin
							res.amp = $
								sqrt((2*!dpi)^3/(wcon2*determ(arrhat)))*inp.amp
							res.amat = asshat - asrhat##arrhinv##arshat
							res.bvc = bvs0 - asrhat##arrhinv##bvr0
							res.con = inp.con0- bvr0##arrhinv##transpose(bvr0)/2
						end
				2	:	begin
							res.amp = $
								sqrt((2*!dpi)^3/(wcon2*determ(asshat)))*inp.amp
							res.amat = arrhat - arshat##asshinv##asrhat
							res.bvc = bvr0 - arshat##asshinv##bvs0
							res.con = inp.con0- bvs0##asshinv##transpose(bvs0)/2
						end
				else:	message, 'No such case!'
			endcase
		endif else begin
			aaainv = SVD_invert(inp.amat,/ref)
			den = wcon2 - (inp.bvc1##aaainv##transpose(inp.bvc1))[0]
			arg = ((inp.bvc0##aaainv##transpose(inp.bvc1))^2/(2*den) + $
				inp.bvc0##aaainv##transpose(inp.bvc0)/2)[0] - inp.con0
			res = inp.amp*sqrt((2*!dpi)^5/(den*determ(inp.amat)))*exp(arg)
		endelse
	endif else begin
		if Streq(tag_names(inp,/str),'iabc') then begin
			aaainv = SVD_invert(inp.amat,/ref)			
			arg = (inp.bvc##aaainv##transpose(inp.bvc)/2)[0] - inp.con
			res = inp.amp*sqrt((2*!dpi)^2/determ(inp.amat))*exp(arg)
		endif else message, 'Primary input must be an ABC or IABC structure!'
	endelse

	return, res
end