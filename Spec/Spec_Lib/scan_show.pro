Pro Scan_show, s_0, s_1, s_2, s_3, s_4, s_5, s_6, s_7, $
	s_8, s_9, s_10, s_11, s_12, s_13, s_14, s_15, $
	columns = cols, hoffset = hof, voffset = vof, scale = scl, extend = exn, $
	count = cnt, err_rat = ert, numbers = nmb, offnum = ofn, noerror = noe, $
	line = line, lcolor = lcol, thick = thick, ynozero = ynoz, wait = wai, $
	output= out, full_siz= ful, half_siz= hlf, quarter_siz= qua, blin_siz= bln,$
	nofile = nof, _extra = _e

;+
; NAME:
;		SCAN_SHOW
; VERSION:
;		8.3
; PURPOSE:
;		Displays the results of a scan or a combination of scans.
; CATEGORY:
;		SPEC data processing.
; CALLING SEQUENCE:
;		SCAN_SHOW, S_0 [, S_1 ...] [, keywords]
; INPUTS:
;	S_0, S_1, .. S_15
;		May be:
;			1)  Scans, i.e. [3,n] arrays.
;			2)	Scan numbers, i.e. integers
;			3)  A single array of scan numbers.
;
;		In the first 2 cases the  maximal number of scans is 15.  In the third
;		case there is no limit.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	COLUMNS
;		Only active when SCAN_SHOW is called with a list of scans (through
;		SCANS). COLUMNS is used to transfer COLS data to SCAN_READ (see there).
;
;		If COLUMNS is not provided, SCAN_READ querries for it, interactively.
;	HOFFSET
;		Numerical scalar or vector, adds horizontal offset to the data.  If 
;		given as scalar, same offset is applied to all scans.  If given as 
;		vector, the number of elements must agree with the number of scans in
;		the scan list.
;	VOFFSET
;		Same as HOFFSET, for vertical offsets.
;	SCALE
;		Numerical scalar of vector, scales the plot value(s).  If given as 
;		scalar, same scaling is applied to all scans.  If given as vector, the 
;		number of elements must agree with the number of scans in the scan list.
;		 
;		 Note:	If VOFFSET is used, SCALE cannot be used.
;	EXTEND
;		Numeric scalar in the range [-1,1].  If given, the x-range of the plot
;		is extended by ABS(EXTEND)*(XMAX - XMIN).  If the value of EXTEND is 
;		negative, the extension is on the left, else it is on the right.
;	/COUNT
;		Switch, specifies count-type data.  If set and /YLOG is set for the
;		plot, 1 is added to the plotted data.
;		Note:	If the data includes negative values, CEIL(ABS(MIN(DATA))) + 1
;				is added, instead of 1.
;	/ERR_RAT
;		Switch.  If set, data is replaced by data errors normalized to data.
;		The errors column is set to zero in this case.  Active when SCAN_SHOW
;		is called with a list of scans (through SCANS).
;	/NUMBERS
;		Switch.  If set, the concecutive numbers of the points in the scans are
;		displayed above the scans.
;	OFFNUM
;		Scalar input, specifies the vertical offset of the point numbers from
;		the points, as a fraction of the vertical range.  OFFNUM is only active
;		when /NUMBERS is set.
;	/NOERROR
;		Switch.  If set, the data is displayed without error bars.
;	LINE
;		Numeric scalar or vector, setting a plot line type.  If given as a
;		vector, consecutive scans are plotted with consecutive entries, if
;		given as a scalar, the provided line type is used for all scans.
;		Defaults to 0.
;	LCOLOR
;		Numeric scalar or vector, setting a line color.  If given as a vector,
;		consecutive scans are plotted with consecutive entries, if given as a
;		scalar, the provided color is used for all scans.  Defaults to 0.
;	THICK
;		Numeric scalar or vector, setting a plot line thickness.  If given as a
;		vector, consecutive scans are plotted with consecutive entries, if
;		given as a scalar, the provided line thickness is used for all scans.
;		Defaults to 1.
;
;		Note:	If the number of line types, colors or thicknesses is smaller 
;		then the number of scans, then the types/colors/thicknesses are recycled
;	/YNOZERO
;		Switch.  Same function as /YNOZERO in the IDL PLOT routine.
;	/WAIT
;		Switch.  When set, the plots are displayed one after another, waiting 
;		for the user to hit a key to continue.  Alternatively, if WAIT is given
;		as negative value, the program waits for the time specified by the 
;		absolute value of WAIT before displaying the next plot.
;	/OUTPUT
;		Switch.  If set, printer output is performed.
;	/FULL_SIZ
;		Switch, if set, the printer output is full size.
;	/HALF_SIZ
;		Switch.  If set, the size of the printer output is (roughly) halved.
;	/QUARTER_SIZ
;		Switch.  If set, the size of the printer output is (roughly) cut to a
;		quarter.
;	/BLIN_SIZ
;		Switch.  If set, the size of the printer output is set to a size
;		intermediate between "half" and "quarter".
;
;		Note:	The four output size keywords are "sticky", meaning that once
;		set the size remains same until changed.
;	/NOFILE
;		Switch.  If set, graphic file generation is disabled.
;	_EXTRA
;		A formal key, used to transfer additional keywords to SCAN_READ,
;		SPEC_FILE_INFO and WIMG_MM.
;
;		Note:	If any of the keywords /PNG, /JPG or /BMP is invoked in call
;				to SCAN_SHOW, whether directly or through _EXTRA, the
;				appropriate image file will be generated by WIMG_MM.
; OUTPUTS:
;		None other than screen (and possibly printer) output.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		Either actual scans or a list of scan numbers may be provided, but not
;		both
; PROCEDURE:
;		Straightforward.  Calls SCAN_LIST_VER, SCAN_OFFSET, SCAN_READ (only when
;		in the list mode) and SCAN_SCALE.  Uses CLEAN_NAME, DEFAULT, ERRBARS, 
;		ISNUM, LABELS, ONE_OF, PLVAR_KEEP, SPLIT_XY, WHERINSTRUCT and WIMG_MM, 
;		from MIDL
; MODIFICATION HISTORY:
;		Created 20-SEP-2002 by Mati Meron.
;		Modified 10-JAN-2004 by Mati Meron.  Added keyword LCOLOR.
;		Modified 20-FEB-2004 by Mati Meron.  Added OUTPUT and related keywords.
;		Modified 15-APR-2004 by Mati Meron.  Changed internal data format.
;		Modified 25-SEP-2004 by Mati Meron.  Added keywords PNG and CHANGE.
;		Modified 15-JUL-2005 by Mati Meron.  Added keyword ERR_RAT.
;		Modified 15-FEB-2005 by Mati Meron.  Added keyword COUNT.
;		Modified 10-NOV-2007 by Mati Meron.  Added keyword HOFFSET.
;		Modified 20-NOV-2007 by Mati Meron.  Removed the /PNG keyword, added
;		the /NOFILE keyword and support for PNG, JPG and BMP.
;		Modified 15-APR-2008 by Mati Meron.  Added keyword /NOERROR.
;		Modified 25-AUG-2008 by Mati Meron.  Added keywords NUMBERS and OFFNUM.
;		Modified 30-SEP-2009 by Mati Meron.  Internal changes, changed the
;		behavior of LINE and LCOLOR and the workings of COUNT.
;		Modified 5-NOV-2009 by Mati Meron.  Increased maximal number of scans
;		from 8 to 16.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;		Modified 25-MAR-2011 by Mati Meron.  Added keyword SCALE.
;		Modified 5-AUG-2011 by Mati Meron.  Added keyword VOFFSET and upgraded
;		the operation of HOFFSET.
;		Modified 20-NOV-2011 by Mati Meron.  Added keyword WAIT.
;		Modified 5-APR-2012 by Mati Meron.  Internal changes.  Added keyword
;		EXTEND.
;		Modified 20-FEB-2014 by Mati Meron.  Added keyword THICK.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1
	snams =  strcompress('s_' + sindgen(16),/remove)

	nsc = Scan_list_ver(s_0,s_1,s_2,s_3,s_4,s_5,s_6,s_7,$
		s_8,s_9,s_10,s_11,s_12,s_13,s_14,s_15,/part,pflag=pfl,flag=lfl,lis=slis)
	if nsc eq 0 then message, 'Missing or inconsistent input!'
	
	case n_elements(hof) of
		0	:	whof = replicate(0.,nsc)
		1	:	whof = replicate(hof,nsc)
		nsc	:	whof = hof
		else:	message, $
				'Number of offset values must equal 1 or number of scans!'
	endcase
	hofl = whof ne 0

	case n_elements(vof) of
		0	:	wvof = replicate(0.,nsc)
		1	:	wvof = replicate(vof,nsc)
		nsc	:	wvof = vof
		else:	message, $
				'Number of offset values must equal 1 or number of scans!'
	endcase
	vofl = wvof ne 0

	if max(vofl) eq 0 then begin
		case n_elements(scl) of
			0	:	wscl = replicate(1.,nsc)
			1	:	wscl = replicate(scl,nsc)
			nsc	:	wscl = scl
			else:	message, $
					'Number of scaling values must equal 1 or number of scans!'
		endcase
	endif else begin
		if n_elements(scl) gt 0 then message, $
		'Scaling not allowed when vertical offset is used', /con
		wscl = replicate(1.,nsc)
	endelse
	scfl = wscl ne 1
	offl = hofl or vofl

	if lfl then scan=Scan_read(slis[0],col=cols,err=ert,head = head,_extra=_e) $
	else scan = s_0
	dum = (Wherinstruct('new',_e))[0]
	if dum ge 0 then _e.(dum) = 0
	if offl[0] then scan = Scan_offset(scan,xoff=whof[0],yoff=wvof[0],part=pfl)
	if scfl[0] then scan = Scan_scale(scan,wscl[0],part=pfl)
	xmin = min(scan[0,*],max=xmax)
	ymin = min(scan[1,*],max=ymax)

	for i = 1, nsc - 1 do begin
		if lfl then scan = Scan_read(slis[i],col=cols,err=ert,_extra=_e) $
		else idum = execute('scan = ' + snams[i])
		if offl[i] then scan = $
		Scan_offset(scan,xoff=whof[i],yoff=wvof[i],part=pfl)
		if scfl[i] then scan = Scan_scale(scan,wscl[i],part=pfl)
		txmin = min(scan[0,*],max=txmax)
		tymin = min(scan[1,*],max=tymax)
		xmin = xmin < txmin
		xmax = xmax > txmax
		ymin = ymin < tymin
		ymax = ymax > tymax
	endfor

	if Isnum(exn) then begin
		pad = (xmax - xmin)*(-1 > exn < 1)
		if pad le 0 then xmin = xmin - pad else xmax = xmax + pad
	endif 

	lin = Default(line,0,/dtyp)
	nlin = n_elements(lin)
	lcl = Default(lcol,0l,/dtyp)
	nlcl = n_elements(lcl)
	thi = Default(thick,1,/dtyp)
	nthi = n_elements(thi)
	ylfl = ((Wherinstruct('ylo',_e))[0] ge 0)
	if not (keyword_set(ynoz) or ylfl) then ymin = ymin < 0
	lcfl = (keyword_set(cnt) and ylfl)*(ceil(abs(ymin < 0)) + 1)
	wai = Default(wai,0.,/dtyp)
	if wai gt 0 then print, string([13b,9b,9b]) + $
	'Hit "Q" to exit, any other key to continue'

	nori = Wherinstruct('nor',_e)
	if nori ge 0 then begin
		nkeep = _e.(nori)
		_e.(nori) = 0
	endif
	outfl = keyword_set(out) + 1
	repn = 0
	repeat begin
		if repn then begin
			hqs = One_of(ful,hlf,qua,bln)
			Plvar_keep, action = 'save'
			set_plot, 'printer'
			device, /landscape, /true_color
			tvlct, red, gre, blu, /get
			case hqs of
				0	:	dum = execute('device,/inch,xsiz=9.5,ysiz=7')
				1	:	dum = execute('device,/inch,/port,xsiz=7,ysiz=5')
				2	:	dum = execute('device,/inch,xsiz=5,ysiz=3.5')
				3	:	dum = execute('device,/inch,xsiz=5,ysiz=5')
				else:
			endcase
		endif else begin
			if (!d.flags and 256)/256 then begin
				if !d.window eq (-1) then wset
				wshow
			endif
		endelse

		if lfl then begin
			tit= fildat.name + ' ; Scan # : ' + $
			strcompress(strjoin(string(slis),', '))
			if Isnum(cols) then begin
				xtit = head[cols[0]]
				ytit = head[cols[1]]
			endif else xtit = (ytit = '')
			plot, [0], xrange = [xmin,xmax], yrange = [ymin,ymax] + lcfl, $
			/nodata, title = tit, xtitle = xtit, ytitle = ytit, _extra = _e
		endif else plot, [0],xrange= [xmin,xmax],yrange= [ymin,ymax]+ lcfl,$
		/nodata, _extra = _e

		efl = not (keyword_set(noe) or pfl)
		for i = 0, nsc - 1 do begin
			if lfl then begin
				pcom = $
				'Scan_read(' + string(slis[i]) + ',col=cols,err=ert,_extra=_e)'
				idum = execute('scan = ' + pcom)
			endif else idum = execute('scan = ' + snams[i])
			if offl[i] then $
			scan = Scan_offset(scan,xoff=whof[i],yoff=wvof[i],part=pfl)
			if scfl[i] then scan = Scan_scale(scan,wscl[i],part=pfl)
			npo = Split_xy(scan,x=x,y=y,z=ser,/keep)
			oplot, x, y + lcfl, line = lin[i mod nlin], col = lcl[i mod nlcl], $
			thick = thi[i mod nthi], _extra = _e
			if efl then Errbars, x, y + lcfl, yerr = ser, $
			col = lcl[i mod nlcl], _extra = _e
			if keyword_set(nmb) then begin
				nlab = lindgen(npo)
				dum = where((nlab mod 10) ne 0,ndum)
				if ndum gt 0 then nlab[dum] = nlab[dum] mod 10
				Labels, x, y+ lcfl+ Default(ofn,0.02,/dtyp)*(ymax-ymin), $
				string(nlab,form='(i0)'),align=0.5,col=!pcol.pink, charsize= 0.8
			endif
			if not ((wai eq 0) or repn or (i eq (nsc-1))) then begin
				if wai gt 0 then begin
					dum = (dum = get_kbrd())
					if Streq(dum,'q',1) then break
				endif else wait, abs(wai)
			endif
		endfor

		if not (repn or keyword_set(nof)) then begin
			dum = (Wherinstruct('tit',_e))[0]
			if dum ge 0 then itit = _e.(dum) else itit = Default(tit,'')
			 Wimg_mm, Clean_name(itit), /nodef, /verb, call = 2, _extra = _e
		endif

		if repn then begin
			device, /close
			Plvar_keep, act = 'restore'
		endif

		repn = repn + 1
	endrep until repn eq outfl
	if nori ge 0 then _e.(nori) = nkeep

	return
end