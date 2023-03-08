Pro Input_reform, hq, vq, whq = whq, wvq = wvq, type = typ, dim = dim

;+
; NAME:
;		INPUT_REFORM
; VERSION:
;		8.47
; PURPOSE:
;		Reforms inputs to common inputs for various scattering calculations.
; CATEGORY:
;		Scattering specific.
; CALLING SEQUENCE:
;		INPUT_REFORM, HQ, VQ, WHQ = WHQ, WVQ = WVQ [, optional keywords]
; INPUTS:
;	HQ
;		The value(s) of the horizontal (inplane) part of a Q-vector.
;	VQ
;		The value(s) of the vertical (Z) component of a Q-vector.
;
;		HQ and VQ are converted to WHQ and WVQ.  A number of input schemes are
;		available, as follows:
;
;			1)	Only HQ given.  In this case HQ needs to be a [K,M,N] array
;				with K >= 2.  The HQ[0,*,*] part is converted to WHQ while
;				HQ[1,*,*] is converted to WVQ.
;			2)	Both inputs provided, as scalars.  Copied as is (with possible
;				numeric type modification).
;			3)	HQ provided as vector and VQ as scalar, or vice versa.  The
;				scalar input will be extended to a vector of same length as the
;				other input, with all components equal to the scalar value.
;			4)	HQ a vector of length M, VQ a vector of length N.  On convertion
;				both vectors will be extended to arrays of dimensions [M,N].
;			5)	HQ and VQ provided as 2D arrays.  Copied as is if both arrays
;				have same dimensions.
;
;			Any other combination is unacceptable.
; OPTIONAL INPUT PARAMETERS:
; 		None.
; KEYWORD PARAMETERS:
; 	WHQ
; 		Output parameter, see in OUTPUTS.
; 	WVQ
; 		Output parameter, see in OUTPUTS.
; 	TYPE
;		Integer scalar, the requested numeric type of the output.  If not given
;		the output type will be the higher of the input types.  In any case the
;		output type will be no lower than 4 (float).
;	DIM
;		Optional output, see below.
; OUTPUTS:
; 		The output is through the return variables WHQ and WVQ which contain
; 		modified and/or extended versions of HQ and VQ.  WHQ and WVQ always have
; 		same dimensions (both scalar, vectors or 2D arrays) and are of same
; 		type.
; OPTIONAL OUTPUT PARAMETERS:
; 	DIM
; 		Returns the (common) dimensions of WHQ, WVQ.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other then the input restrictions on HQ and VQ, as listed above.
; PROCEDURE:
;		Straightforward.  Calls ARREQ, CALCTYPE, DEFAULT, CAST and NPARDEF,
;		from MIDL.
; MODIFICATION HISTORY:
;		Created 25-JUL-2016 by Mati Meron.
;-

	on_error, 1

	rtyp = Default(typ,Calctype(hq,vq,0.,def=4),/dtyp) > 4

	case Npardef(hq,vq) of
		0	:	message, 'Missing Q-inputs!'
		1	:	begin
					whq = reform(hq)
					siz = size(whq)
					if siz[0] eq 3 then begin
						if siz[1] ge 2 then begin
							wvq = reform(whq[1,*,*])
							whq = reform(whq[0,*,*])
						endif else message, 'Insufficient Q-input!'
					endif else message, 'Bad Q-input'
				end
		2	:	begin
					if n_elements(hq) eq 1 then whq = hq[0] $
					else whq = reform(hq)
					if n_elements(vq) eq 1 then wvq = vq[0] $
					else wvq = reform(vq)
					sf = size(whq)
					ss = size(wvq)
					case sf[0]^2 + ss[0]^2 of
						0	:
						1	:	begin
									if sf[0] eq 1 then $
									wvq = replicate(wvq,sf[1]) $
									else whq = replicate(whq,ss[1])
								end
						2	:	begin
									whq = whq#replicate(1.,ss[1])
									wvq = replicate(1.,sf[1])#wvq
								end
						8	:	if not Arreq(sf[1:2],ss[1:2]) then $
								message, 'Incompatible Q-dimensions!'
						else:	message, 'Unacceptable Q-inputs!'
					endcase
				end
		else:	message, 'Too many Q-inputs!'
	endcase

	whq = Cast(whq,rtyp,rtyp)
	wvq = Cast(wvq,rtyp,rtyp)
	dim = size(whq,/dim)

	return
end