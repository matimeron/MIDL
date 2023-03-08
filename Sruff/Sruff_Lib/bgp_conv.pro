Function BGP_conv, field= bfl, gap= gap, period= per, scu= scu, reload=rel,$
	_extra = _e

;+
; NAME:
;		BGP_CONV
; VERSION:
;		8.72
; PURPOSE:
;		Given two of the values field, gap, period, calculates the third.
; CATEGORY:
;		SR specific
; CALLING SEQUENCE:
;		Result = BGP_CONV ( keywords)
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
;	/SCU
;		Switch.  Specifies that superconducting (NbTi) magnets are used.
;	/RELOAD
;		Switch.  If set, the field data (see COMMON BLOCKS) is reloaded.  The
;		data is normally loaded the first time the the routine is called, the
;		only case it may need to be reloaded is if the data has been changed
;		without exiting IDL.
;	_EXTRA
;		Formal keyword used to pass keyword to embedded routines.  Not to be
;		used directly.
; OUTPUTS:
;		For whatever two out of the three input parameters are provided, returns
;		the value(s) of the third one.
; OPTIONAL OUTPUT PARAMETERS:
; 		None.
; COMMON BLOCKS:
;		FIELD_DATA.  See routine LOAD_FIELD_DATA for details.
; SIDE EFFECTS:
;		As mentioned above, when one of the inputs is a scalar and the other an
;		arrray, the scalar is reformated as an array.  Also, on return all
;		parameters are of type no lower than 4 (float).
; RESTRICTIONS:
;		The calculations of field values have a restricted range (see routine
;		LOAD_FIELD_DATA for details).  For out of range inputs, Nan will be
;		returned.
; PROCEDURE:
; 		Performs either direct or reverse interpolation in the appropriate
; 		field data table.
;		Calls LOAD_FIELD_DATA.  Calls CALCTYPE, CODIMS, DEFAULT, HOW_MANY,
;		IN2D_INV and INTER_2D and WHERINSTRUCT, from MIDL.
; MODIFICATION HISTORY:
;		Created 25-OCT-2015 by Mati Meron, based on ID_CONV.
;		Completely rewritten 1-JUN-2020 by Mati Meron.
;		Modified 10-JUL-2020 by Mati Meron.  Internal changes.
;-

	common field_data, nfl, bpmu, bscu, blim, glim, plim, dind, las, ilas
	on_error, 1

	Load_field_data, scu = scu, reload = rel, data = dat
	
	if Codims(Default(bfl,gap),Default(gap,per),Default(per,bfl), $
	dims=np,same=same) then begin
		np = np[0] > 1
		res = make_array(np,typ=Calctype(bfl,gap,per,def=4))
	endif else message, $
	'Only vector-scalar or same length vector inputs allowed!'
	wha = How_many(fir=bfl,sec=gap,thi=per,whi=whi)

	efl = ((Wherinstruct('ext',_e))[0] ge 0)
	wind = intarr(3)
	if (whi mod 2 eq 1) then $
	wind[0] = min(bfl) lt blim[0] or max(bfl) gt blim[1]
	if (whi/2 mod 2 eq 1) then $
	wind[1] = min(gap) lt glim[0] or max(gap) gt glim[1]
	if (whi/4 mod 2 eq 1) then $
	wind[2] = min(per) lt plim[0] or max(per) gt plim[1]
	if max(wind) gt 0 then begin
		gstr = ' input out of range. Tabulated range is '
		print
		if wind[0] then print, 'Field' + gstr, blim, $
		form = '("	",a,"[ ",f7.4,", ",f7.4," ]")'
		if wind[1] then print, 'Gap' + gstr, glim, $
		form = '("	",a,"[ ",f7.4,", ",f7.4," ]")'
		if wind[2] then print, 'Period' + gstr, plim, $
		form = '("	",a,"[ ",f7.4,", ",f7.4," ]")'
		print
		if efl then message, '	Extrapolating, questionable accuracy', /con $
		else message, 'Cannot continue, exiting!'
	endif

	case whi of
		3	:	begin
					if same then begin
						for i = 0, np-1 do res[i] = In2d_inv( $
						xval=gap[i],fval=bfl[i],z_sor=dat,las=las,_extra=_e)
					endif else begin
						if n_elements(gap) eq 1 then res = In2d_inv( $
						xval=gap,fval=bfl,z_sor=dat,las=las,_extra=_e) $
						else for i = 0, np-1 do res[i] = In2d_inv($
						xval=gap[i],fval=bfl,z_sor=dat,las=las,_extra=_e)
					endelse
				end
		5	:	begin
					if same then begin
						for i = 0, np-1 do res[i] = In2d_inv( $
						yval=per[i],fval=bfl[i],z_sor=dat,las=las,_extra=_e)
					endif else begin
						if n_elements(per) eq 1 then res = In2d_inv( $
						yval=per,fval=bfl,z_sor=dat,las=las,_extra=_e) $
						else for i = 0, np-1 do res[i] = In2d_inv($
						yval=per[i],fval=bfl,z_sor=dat,las=las,_extra=_e)
					endelse
				end
		6	:	begin
					if same then for i = 0, np-1 do res[i] = Inter_2d( $
					gap[i],per[i],z_sor=dat,las=las,_extra=_e) else $
					res = Inter_2d(gap,per,z_sor=dat,/vec,las=las,_extra=_e)
				end
		else:	message, 'Two and only two inputs requested!'
	endcase

	if n_elements(res) eq 1 then res = res[0]
	ilas = 1
	return, res
end