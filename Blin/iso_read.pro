Function Iso_read, file, adat = adt, bdat = bdt, raw = raw, thresh = tre, $
	equalize = equ, order = ord, bin= bin, width= wid, show= sho, spline= spc, $
	_extra = _e

;+
; NAME:
;		ISO_READ
; VERSION:
;		8.07
; PURPOSE:
; 		Reads and, optionally, processes isotherm files.
; CATEGORY:
;		Isotherm data input.
; CALLING SEQUENCE:
;		Result = ISO_READ( [FILE] [, keywords])
; INPUTS:
;	FILE
;		File name, if not given will be querried for interactively.
; OPTIONAL INPUT PARAMETERS:
; 		None.
; KEYWORD PARAMETERS:
; 	/ADAT												|	At most one of these
; 		Switch.  Specifies that pressure is to be read	|	two keywords may be
; 		from the PiA column.							|	set.  If none is,the
; 	/BDAT												|	default is reading
; 		Switch.  Specifies that pressure is to be read	|	from the PiA column.
; 		from the PiB column.							|
; 	/RAW
; 		Switch.  If set,  the data is  read as is, with no processing.  The 
; 		default is to purge from the data the regions where the area is not 
; 		changing.
; 	THRESH
; 		Numeric scalar, sets the minimal value of area change (see above) which 
; 		is considered greater than 0.  Default THRESH value is 0.01 (cm^2).
; 	/EQUALIZE
; 		Switch.  If set, the area values are fitted to the time values present
; 		in the data, to eliminate systematic errors.
; 	ORDER
; 		Integer scalar, determines the order of polynomial fitting when 
; 		/EQUALIZE is set.  Default value is 3.   If /EQUALIZE is not set, ORDER
; 		has no effect.
; 	BIN
; 		Integer scalar.  If given and >1, the data binned with the average size
; 		of a bin equaling the value of BIN.
; 	WIDTH
; 		Integer scalar.  If given and >1, the data is smoothed using binomial
; 		kernel of width WIDTH.
; 		
; 		Note:	The order of data processing is:
; 					1)	Equalizing.
; 					2)	Binning.
; 					3	Smoothing.
; 				No processing is  done if /RAW is set.
; 	/SHOW
; 		Switch.  If set, the data, following any ordered processing,is 
; 		displayed to the screen.
; 	SPLINE
; 		Optional output, see below.
;	_EXTRA
;		A formal keyword used to pass keywords to imbedded routines.  Not to be
;		used directly.
; OUTPUTS:
;		Returns the read and, optionally processed isotherm data as a 2-column
;		array, where the first column contains the area values and the second, 
;		the pressure values.
; OPTIONAL OUTPUT PARAMETERS:
;	SPLINE
;		If provided with a name of a variable (doesn't need to be defined) 
;		returns the spline coefficient array for the output data.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
; 		Straightforward, reads the data and processes it as needed.  Calls
; 		PEAK_SHOW and PEAK_SMOOTH from SPEC.  Calls ADC, DEFAULT, DIF, 
; 		LINFIT_MM, ONE_OFF, POLEVAL, RASCII, SPLIN_COEFFS, STRMATCH_MM and 
; 		STRPARSE_MM, from MIDL.
; MODIFICATION HISTORY:
;		Created 10-JUN-2011 by Mati Meron.
;-

	on_error, 1
	
	dcol = ['AreaA,cm','PiA,mN/m','PiB,mN/m','Time']
	tre = Default(tre,0.01)
	wwid = Default(wid,1,/dtyp)
	wbin = Default(bin,1,/dtyp)
	ab = (One_of(adt,bdt) > 0)

	dat = Rascii(file,head=head,_extra=_e)
	dum = Strparse_mm(head[0],'	 ',lis)
	aind = Strmatch_mm(dcol[0],lis,strlen(dcol[0]))
	pind = Strmatch_mm(dcol[ab+1],lis)
	tind = Strmatch_mm(dcol[3],lis,strlen(dcol[3]))
	are = reverse(reform(dat[aind,*]))
	pre = reverse(reform(dat[pind,*]))
	tim = reverse(reform(dat[tind,*]))
	
	if not keyword_set(raw) then begin
		dar = Dif(are,/edg)
		dum = where(dar gt tre, dlen)
		if dlen gt 0 then begin
			are = are[dum]
			pre = pre[dum]
			tim = tim[dum]
		endif else message, 'No useful data present!'

		if keyword_set(equ) then begin
			tmin = min(tim,max=tmax)
			ftim = (2*tim - tmax - tmin)/(tmax - tmin)
			coe = Linfit_mm(ftim,are,ord=Default(ord,3,/dtyp)>1)
			are = Poleval(ftim,coe)
		endif

		if wbin gt 1 then begin
			nbn = round(dlen/wbin)
			tem = ADC(are,n_chan=nbn,val=bar,rev=rev)
			bpr = 0*bar
			lo = rev[0:nbn-1]
			hi = rev[1:nbn] - 1
			net = hi - lo + 1
			for i = 0, nbn-1 do begin
				if net[i] gt 0 then bpr[i]=total(pre[rev[lo[i]:hi[i]]])/(net[i])
			endfor
			dum = where(net gt 0)
			are = bar[dum]
			pre = bpr[dum]
		endif
		res = transpose([[are],[pre]])
		if wwid gt 1 then res = Peak_smooth(res,wid=wwid,_extra=_e)
	endif else res = transpose([[are],[pre]])
	if arg_present(spc) then spc = Splin_coeffs(res[0,*],res[1,*])		

	if keyword_set(sho) then Peak_show, res, _extra = _e

	return, res
end