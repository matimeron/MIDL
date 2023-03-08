Function Scan_read, snum, columns = cols, tau = tau, absorb= abb, no_err= ner,$
	 err_rat= ert, unsorted= uns, ifirst= ifr, header=head, time=tim, _extra= _e

;+
; NAME:
;		SCAN_READ
; VERSION:
;		8.02
; PURPOSE:
;		Reads specified X and Y data columns of one scan from a SPEC data file.
; CATEGORY:
;		SPEC Input/Output.
; CALLING SEQUENCE:
;		Result = SCAN_READ( SNUM [, COLUMNS = COLS] [, keywords ])
; INPUTS:
;	SNUM
;		Positive integer, scan #, mandatory.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	COLUMNS
;		An integer vector with 2 or 3 entries.  The first two entries are the
;		numbers of the X and Y columns (either counting from 0 up, from left,
;		or from -1 down, from right).  If a third entry is provided, it is
;		used as the number of a normalization column.
;
;		Alternatively, COLUMNS may be given as a character vector, with 2 or 3
;		entries which should correspond to names in the data header.
;
;		If COLUMNS is not provided, the routine querries for it, interactively.
;	TAU
;		Scalar value, detector time constant (in seconds).  If provided and non
;		zero, the data is corrected for pileup.
;	/ABSORB
;		Switch.  If set, data is corrected for absorption (not active yet).
;	/NO_ERR
;		Switch.  If set, statistical errors are not calculated.
;	/ERR_RAT
;		Switch.  If set, data is replaced by data errors normalized to data.
;		The errors column is set to zero in this case.
;	/UNSORTED
;		Switch.  If set, the data is not sorted in ascending order of the
;		X column.  Default is SORT.
;	/IFIRST
;		Switch.  If set, the first column of the output (the X values) is 
;		replaced by the data point's consecutive numbers, 0, 1, ...etc.
;	HEADER
;		Optional output, see below.
;	TIME
;		Optional output, see below.
; OUTPUTS:
;		Returns the data as a [3,n] array, where n is the number of scan
;		points.  The 3 columns, in order, are X, Y, error(Y).  If /NO_ERR is
;		set, the error(Y) column is filled with zeroes.
;	_EXTRA
;		A formal keyword used to pass keywords to SPEC_FILE_INFO. All
;		SPEC_FILE_INFO keywords are accepted.  Not to be used directly.
; OPTIONAL OUTPUT PARAMETERS:
;	HEADER
;		Returns the data header line (#L line in the file) parsed into a
;		character array.
;	TIME
;		Returns a vector containing the time values (for the individual data
;		points) if a column with the title "seconds' ('sec' is enough) exists,
;		else returns a vector of zeroes.  If a normalization column is defined,
;		the time vector is divided by it, same as the data.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Uses SCAN_GET to obtain the scan data, then selects the subset
;		corresponding to the provided column numbers.  Sorts, as needed, using
;		SCAN_SORT.  Calls SCAN_PILE_COR if needed.  Calls ARREQ, ISNUM, STREQ,
;		STRMATCH_MM, TOLER and TYPE from MIDL.
; MODIFICATION HISTORY:
;		Created 5-SEP-2002 by Mati Meron.
;		Modified 25-FEB-2004 by Mati Meron, added keywords TAU and TIME.
;		Modified 15-APR-2004 by Mati Meron.  Changed last column from squares
;		of errors into plain error values.
;		Modified 15-JUL-2005 by Mati Meron.  Added keyword ERR_RAT.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;		Modified 25-FEB-2011 by Mati Meron.  Added keyword IFIRST.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	nc = n_elements(cols)
	if nc eq 1 or nc gt 3 then message, 'Provide 2-3 column numbers, or none!'
	scan = Scan_get(snum, header = head, status = stat,_extra = _e)

	if stat then begin
		nh = n_elements(head)
		if nc ne 0 then begin
			if Type(cols) eq 7 then begin
				wxc = Strmatch_mm(cols[0],head)
				xfl = (wxc ge 0)
				wyc = Strmatch_mm(cols[1],head)
				yfl = (wyc ge 0)
				if nc eq 3 then begin
					wnc = Strmatch_mm(cols[2],head)
					nfl = (wnc ge 0)
				endif else nfl = 3b
			endif else begin
				if Isnum(cols,/int) then begin
					wcols = (cols + nh) mod nh
					wxc = wcols[0]
					xfl = wxc ge 0 and wxc lt nh
					wyc = wcols[1]
					yfl = wyc ge 0 and wyc lt nh
					if nc eq 3 then begin
						wnc = wcols[2]
						nfl = wnc ge 0 and wnc lt nh
					endif else nfl = 3b
				endif else message, 'Unacceptable input!'
			endelse
		endif else begin
			xfl = (yfl = 0b)
			nfl = 2b
		endelse

		if not (xfl and yfl and nfl) then begin
			whead = ' ' + head + ' '
			hlen = strlen(whead)
			pind = strcompress(string(indgen(nh)),/remove)
			nind = strcompress(string(indgen(nh) - nh),/remove)
			blank = string(replicate(32b,max(hlen)))
			phead = (nhead = whead)
			for i = 0, nh-1 do begin
				fdum = blank
				strput, fdum, pind[i], hlen[i]/2
				sdum = phead[i]
				strput, sdum, fdum
				phead[i] = sdum
				fdum = blank
				strput, fdum, nind[i], hlen[i]/2 - 1
				sdum = nhead[i]
				strput, sdum, fdum
				nhead[i] = sdum
			endfor
			print
			print, strjoin(phead)
			print, strjoin(whead)
			print, strjoin(nhead)
			print

			case xfl + 2*yfl of
				0	:	begin
							wxc = (wyc = 0l)
							read, wxc, wyc, prompt = 'Enter #X, #Y:	'
						end
				1	:	begin
							wyc = 0l
							read, wyc, prompt = 'Enter #Y:	'
						end
				2	:	begin
							wxc = 0l
							read, wxc, prompt = 'Enter #X:	'
						end
				3	:
			endcase
			wxc = (wxc + nh) mod nh
			wyc = (wyc + nh) mod nh

			if not nfl then begin
				if nfl/2 then begin
					qu = ''
					read, qu, prompt = 'Normalization Y/N?:	'
					if Streq(qu,'y',1) or Streq(qu,'',1) then begin
						wnc = 0l
						read, wnc, prompt = 'Enter #N (normalization):	'
						wnc = (wnc + nh) mod nh
						nfl = 1b
					endif
				endif
			endif
		endif

		nfl = nfl eq 1
		res = scan[[wxc,wyc,wyc],*]
		if fildat.scan[snum].ncr[1] eq 1 then res = reform(res,3,1)
		if keyword_set(ner) then res[2,*]= 0 else res[2,*]= sqrt(abs(res[2,*]))
		if nfl then res[1:2,*] = res[1:2,*]/scan[[wnc,wnc],*]
		if not keyword_set(uns) then res = Scan_sort(res,sor=sor)
		;if keyword_set(abb) then begin
		;endif
		if Arreq((size(res))([0,1]),[1,3]) then res = reform(res,[3,1])
		if nfl then cols = [wxc,wyc,wnc] else cols = [wxc,wyc]

		wnt = Strmatch_mm('sec',head,3)
		if wnt ge 0 then tim = reform(scan[wnt,*]) $
		else tim = 0*reform(scan[0,*])
		if nfl then tim = tim/scan[wnc,*]
		if not keyword_set(uns) then tim = tim[sor]

		if Isnum(tau) then if tau gt 0 then res = Scan_pile_cor(res,tim,tau=tau)

		if keyword_set(ert) then begin
			abdat = abs(res[1,*])
			dum = where(abdat gt 0,ndum)
			if ndum gt 0 then sca = min(abdat[dum]) else sca = 1
			res[1,*] = res[2,*]/(abdat > sca*Toler())
			res[2,*] = 0
		endif

		if keyword_set(ifr) then begin
			res[0,*] = findgen(fildat.scan[snum].ncr[1])
			head[wxc] = 'Frame #'
		endif
	endif else res = 0b

	return, res
end