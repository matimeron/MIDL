Function QBPM_cur_get, bpm = bpm

	on_error, 1

	if n_elements(bpm) eq 0 then message, 'Which BPM?'
	pvnam = '15IDA:BPM' + string(bpm, form='(i0)') + ':CURRENT_' + $
	string([1,2,3,4],form='(i0)') + '.VAL'
	pvnam = strcompress(pvnam,/rem)
	
	res = lonarr(4)
	cval = 0l
	for i = 0, 3 do begin
		dum = caget(pvnam[i],cval)
		res[i] = cval
	endfor

	return, res
end