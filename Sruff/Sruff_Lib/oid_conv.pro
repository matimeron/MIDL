Pro OID_conv, field= bfl, gap= gap, period= per, k= kvl, emax= emx, ene= ene, $
	mask= msk, ring_energy= ren, rgam= rgm, enhanced= enh, scu= scu, show= sho,$
	num = ndat, _extra = _e

;+
; NAME:
;		OID_CONV
; VERSION:
;		8.44
; PURPOSE:
;		Conversion between various insertion device parameters.
; CATEGORY:
;		SR specific
; CALLING SEQUENCE:
;		Result = OID_CONV, keywords
; INPUTS:
;		None
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	FIELD										|	These 6 parameters serve
;		The magnetic, in Tesla.					|	as both input and output
;	GAP											|	parameters.  Only two can
;		ID gap, in mm.							|	be provided as inputs, at
;	PERIOD										|	any given time (unless
;		ID period, in mm.						|	MASK is used, see below),
;	K											|	but all six will be outputs.
;		The K value of the ID.					|
;	EMAX										|
;		The maximal (saturation) energy, in keV.|
;	ENE											|
;		Current energy, in keV.					|
;
;	Note:	One (and no more than one) of the inputs may be an array.  In such
;			case all outputs will be arrays of same size.
;	MASK
;		An optional mask which can be used to force OID_CONV to ignore some
;		of the inputs.  Provided as numerical array, any 0 value translates to
;		"ignore" (and any nonzero value to "accept").  See the routine HOW_MANY
;		in MIDL_LIB for details.
;
;		Alternatively, the mask can also be provided as a character array,
;		specifying explicitly the two inputs to be used, out of the list
;		['FIELD','GAP','PERIOD','K','EMAX','ENE'].  Only the first 2 characters
;		are needed and case or order do not matter.
;
;		Note:	When MASK is used, one can have more than 2 defined inputs.
;				The number of inputs which are both "defined" and "accepted"
;				(i.e. corresponding to nonzero MASK entries) must still be 2.
;	RING_ENERGY													|
;		Storage ring energy, in GeV.  Optional, if not provided,| One and only
;		 the APS value (7 GeV) is used.							| one of these
;	RGAM														| may be used.
;		The relativistic Gamma of the stored electron.  Optional|
;	/ENHANCED
;		Switch, obsolete.  Maintained for compatibility with past versions.
;	/SCU
;		Switch.  If set, the calculation is for superconducting (NbTi) magnets.
;	/SHOW
;		Switch.  If set, the calculated results are displayed to the screen,
;		in tabular form.
;	NUM
;		Optional output, see below.
;	_EXTRA
;		A formal keyword used to accept keywords passed indirectly.  Not to be
;		used directly.
; OUTPUTS:
;		See Optional Output Parameters, below.
; OPTIONAL OUTPUT PARAMETERS:
;	FIELD, GAP, PERIOD, K, EMAX, ENE
;		All six input parameters serve as output parameters as well.  Given
;		any two inputs, the cooresponding values for the remaining four
;		parameters are calculated and returned.  If one of the inputs is an
;		array, all the calculated parameters are returned in this format and
;		the second (scalar) input is reformated as an array as well.
;	NUM
;		Returns the length (number of elements) of the data.  As mentioned
;		above, all six return variables have same length.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		As mentioned above, when one of the inputs is a scalar and the other an
;		array, the scalar is reformated as an array.  Also, on return all
;		parameters are of type no lower than 4 (float).
; RESTRICTIONS:
;		1)	The calculations of field values have a restricted range (see
;			OID_FIELD for details).
;		2)	Combinations of inputs which are equivalent to ENE > EMAX are
;			unphysical and do not allow for a calculation.
;		3)	The specific input combination containing PERIOD and EMAX is
;			inadequate as these two parameters are not independent.
;
;		In the first 2 cases no error message will be issued but NaN will be
;		returned for incalculable values.  In the last case, the routine will
;		exit with an error message.
; PROCEDURE:
;		Follows the basic definitions OF SR quantities.  Uses OBGP_CONV, 
;		BL_DEFAULTS and OID_FIELD from SRUFF_LIB, as well as OID_CONV_FUN (from
;		same location) for internal evaluations.  Calls CALCTYPE, CAST, 
;		HOW_MANY, IMAGINARY_MM, ISNUM, ONE_OF, ROOT, STRMATCH_MM, TABULATE and
;		TYPE, from MIDL.
; MODIFICATION HISTORY:
;		Created 20-SEP-2005 by Mati Meron.
;		Modified 15-NOV-2005 by Mati Meron.  Added option of string valued mask.
;		Modified 20-NOV-2006 by Mati Meron.  Internal changes.
;		Modified 25-OCT-2015 by Mati Meron to accomodate superconducting 
;		undulators.  Added keyword SCU.
;		Modified 10-JAN-2016 by Mati Meron.  Internal changes.
;		Obsoleted and renamed OID_CONV 15-JUL-2020 by Mati Meron.
;-

	on_error, 1
	mult = 1e3
	posib = ['field','gap','period','k','emax','ene']
	typ = Calctype(0.,bfl,gap,per,kvl,emx,ene,def=4)

	case One_of(ren,rgm,val=val) of
		-1	:	BL_defaults, gam = wrgm
		0	:	wrgm = 1e6*ren/(!srcon.scal*!srcon.ee)
		1	:	wrgm = rgm
	endcase
	a = Cast(float(2*mult*!pi*!srcon.bere),typ,typ)
	b = Cast($
	float(4*mult*!pi*wrgm^2*!srcon.scal*!srcon.re*!srcon.ee/!srcon.alp),typ,typ)
	sfl = keyword_set(scu)

	if Type(msk) eq 7 then begin
		wmsk = intarr(n_elements(posib))
		for i = 0, n_elements(msk) - 1 do begin
			j = Strmatch_mm(msk[i],posib,2)
			if j ge 0 then wmsk[j] = 1
		endfor
	endif else begin
		if Isnum(msk) then begin
			if n_elements(msk) eq 6 then wmsk = msk $
			else message, 'Mask needs 6 elements!'
		endif
	endelse

	check = [n_elements(bfl),n_elements(gap),n_elements(per),$
			 n_elements(kvl),n_elements(emx),n_elements(ene)]
	if Isnum(wmsk) then check = check*wmsk
	dum = where(check ne 0, ndum)
	case ndum of
		1	:	begin
					if sfl and (dum eq 0 or dum eq 2) then begin
						gap = !blpar.sgap
						check[1] = (sgfl = 1)
						if Isnum(wmsk) then wmsk[1] = 1
					endif else message, 'Insufficient input!'
				end
		2	:	sgfl = 0
		else:	message, 'Too many inputs!'
	endcase

	dum = where(check gt 1, ndum)
	if ndum le 1 then ndat = max(check) $
	else message, 'At most one input can be an array!'
	if ndat gt 1 then begin
		cc = check eq 1
		if cc[0] then bfl = replicate(bfl,ndat)
		if cc[1] then gap = replicate(gap,ndat)
		if cc[2] then per = replicate(per,ndat)
		if cc[3] then kvl = replicate(kvl,ndat)
		if cc[4] then emx = replicate(emx,ndat)
		if cc[5] then ene = replicate(ene,ndat)
	endif

	wha = $
	How_many(fir=bfl,sec=gap,thi=per,fou=kvl,fif=emx,six=ene,mask=wmsk,whi=whi)
	case whi of
		3	:	begin
					bfl = Cast(bfl,typ)
					gap = Cast(gap,typ)
					per = OBGP_conv(field=bfl,gap=gap,scu=sfl,_extra=_e)
					kvl = bfl*per/a
					emx = b/per
					ene = emx/(1 + kvl^2/2)
				end
		5	:	begin
					bfl = Cast(bfl,typ)
					per = Cast(per,typ)
					gap = OBGP_conv(field=bfl,period=per,scu=sfl,_extra=_e)
					kvl = bfl*per/a
					emx = b/per
					ene = emx/(1 + kvl^2/2)
				end
		6	:	begin
					gap = Cast(gap,typ)
					per = Cast(per,typ)
					bfl = OBGP_conv(gap=gap,period=per,scu=sfl,_extra=_e)
					kvl = bfl*per/a
					emx = b/per
					ene = emx/(1 + kvl^2/2)
				end
		9	:	begin
					bfl = Cast(bfl,typ)
					kvl = Cast(kvl,typ)
					per = a*kvl/bfl
					gap = OBGP_conv(field=bfl,period=per,scu=sfl,_extra=_e)
					emx = b/per
					ene = emx/(1 + kvl^2/2)
				end
		10	:	begin
					gap = Cast(gap,typ)
					kvl = Cast(kvl,typ)
					par1 = gap/(a*kvl)
					bfl = 0*par1
					dum = OID_field(1,/inv,scu=sfl,gap=gap,ran=ran,_extra=_e)
					par = [whi,0.,0.]
					for i = 0l, ndat - 1 do begin
						par[1] = par1[i]
						bfl[i]= Root('oid_conv_fun',$
						ran,par=par,scu=sfl,gap=gap[i],stat=stat,_extra=_e)
						if not stat then bfl[i] = !values.f_nan
					endfor
					per = a*kvl/bfl
					emx = b/per
					ene = emx/(1 + kvl^2/2)
				end
		12	:	begin
					per = Cast(per,typ)
					kvl = Cast(kvl,typ)
					bfl = a*kvl/per
					gap = OBGP_conv(field=bfl,period=per,scu=sfl,_extra=_e)
					emx = b/per
					ene = emx/(1 + kvl^2/2)
				end
		17	:	begin
					bfl = Cast(bfl,typ)
					emx = Cast(emx,typ)
					per = b/emx
					gap = OBGP_conv(field=bfl,period=per,scu=sfl,_extra=_e)
					kvl = bfl*per/a
					ene = emx/(1 + kvl^2/2)
				end
		18	:	begin
					gap = Cast(gap,typ)
					emx = Cast(emx,typ)
					per = b/emx
					bfl = OBGP_conv(gap=gap,period=per,scu=sfl,_extra=_e)
					kvl = bfl*per/a
					ene = emx/(1 + kvl^2/2)
				end
		20	:	message, 'No solution for this case!'
		24	:	begin
					kvl = Cast(kvl,typ)
					emx = Cast(emx,typ)
					per = b/emx
					bfl = a*kvl/per
					gap = OBGP_conv(field=bfl,period=per,scu=sfl,_extra=_e)
					ene = emx/(1 + kvl^2/2)
				end
		33	:	begin
					bfl = Cast(bfl,typ)
					ene = Cast(ene,typ)
					if typ eq 4 then tem=complex(0,(3./2)^(3./2)*b*bfl/(a*ene))$
					else tem = dcomplex(0,(3./2)^(3./2)*b*bfl/(a*ene))
					kvl = 2*sqrt(2./3)*sinh(imaginary_mm(asin(tem)/3))
					per = a*kvl/bfl
					gap = OBGP_conv(field=bfl,period=per,scu=sfl,_extra=_e)
					emx = b/per
				end
		34	:	begin
					gap = Cast(gap,typ)
					ene = Cast(ene,typ)
					par1 = gap*ene/b
					par2 = gap/a
					bfl = 0*par1
					dum = OID_field(1,/inv,scu=sfl,gap=gap,ran=ran,_extra=_e)
					par = [whi,0.,0]
					for i = 0l, ndat - 1 do begin
						par[1:2] = [par1[i],par2[i]]
						bfl[i]= Root('oid_conv_fun',$
						ran,par=par,scu=sfl,gap=gap[i],stat=stat,_extra=_e)
						if not stat then bfl[i] = !values.f_nan
					endfor
					per = OBGP_conv(field=bfl,gap=gap,scu=sfl,_extra=_e)
					kvl = bfl*per/a
					emx = b/per
				end
		36	:	begin
					per = Cast(per,typ)
					ene = Cast(ene,typ)
					kvl = (bfl = (gap = (emx = make_array(ndat,typ=typ))))
					arg = 2*(b/(per*ene) - 1)
					good = where(arg ge 0, ngood,comp=bad,ncomp=nbad)
					if ngood gt 0 then begin
						kvl[good] = sqrt(arg[good])
						bfl[good] = a*kvl[good]/per[good]
						gap[good] = OBGP_conv($
						field=bfl[good],period=per[good],scu=sfl,_extra=_e)
						emx[good] = b/per[good]
					endif
					if nbad gt 0 then $
					kvl[bad]=(bfl[bad]=(gap[bad]=(emx[bad]=!values.f_nan)))
				end
		40	:	begin
					kvl = Cast(kvl,typ)
					ene = Cast(ene,typ)
					per = b/(ene*(1 + kvl^2/2.))
					bfl = a*kvl/per
					gap = OBGP_conv(field=bfl,period=per,scu=sfl,_extra=_e)
					emx = b/per
				end
		48	:	begin
					emx = Cast(emx,typ)
					ene = Cast(ene,typ)
					bfl = (gap = (per = (kvl = make_array(ndat,typ=typ))))
					arg = 2*(emx/ene - 1)
					good = where(arg ge 0, ngood,comp=bad,ncomp=nbad)
					if ngood gt 0 then begin
						kvl[good] = sqrt(arg[good])
						per[good] = b/emx[good]
						bfl[good] = a*kvl[good]/per[good]
						gap[good] = OBGP_conv($
						field=bfl[good],period=per[good],scu=sfl,_extra=_e)
					endif
					if nbad gt 0 then $
					bfl[bad]=(gap[bad]=(per[bad]=(kvl[bad]=!values.f_nan)))
				end
		else:	message, 'Unacceptable input, 2 and only 2 params needed!'
	endcase

	if ndat eq 1 then begin
		bfl = bfl[0]
		gap = gap[0]
		per = per[0]
		kvl = kvl[0]
		emx = emx[0]
		ene = ene[0]
	endif

	if keyword_set(sho) then begin
		print
		head=['Field (T)','Gap (mm)','Period (mm)','K  ','Emax (keV)','E (keV)']
		if sfl then begin
			if sgfl then head[1] = 'Def Gap (mm)' else head[1] = 'Eff Gap (mm)'
		endif
		Tabulate, bfl,gap,per,kvl,emx,ene, form= replicate('f8.3',6),head=head
	endif

	return
end