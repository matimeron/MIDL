Pro Pin_range, snum

	on_error, 1

	print
	print, '	Q_xy range:	 ', PD_pin_range(snum,/span)
	print, '	Angle range:', PD_pin_range(snum,/ang,/span)
	print, '	Pixel range:', PD_pin_range(snum,/raw,/span)
	print

	return
end

