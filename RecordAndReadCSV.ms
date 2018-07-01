-- Josh Brewster 3/2018
-- This script is for recording basic data in animation hierarchies. 
-- Call recordChildren $'object' in Listener window and select a directory to output data to a csv easily

-- Collects all children of the inputted parent object. This will be useful if we want to store key frame data easily.
--  obj is in $'name' format.
fn collectMyChildren obj includeParent:true =
(
	-- start collecting direct children of <obj>
	myChildren = #(obj)

	-- add children and their children etc recursively
	i = 0
	while i < myChildren.count do
	(
	i += 1
	join myChildren myChildren[i].children
	)

	-- remove initial object if <includeParent> == false
	if not includeParent then deleteitem myChildren 1

	-- return array containing children
	myChildren
)

-- returns object from array, e.g. an array containing key frame data.
fn getObjFromArray arr obj = (
	
	Obj = arr[findItem arr obj]
	
	Obj
)

-- returns relative position between two objects
fn relPos obj1 obj2 = (
	
	relPosition = #(obj1.pos.x - obj2.pos.x, obj1.pos.y - obj2.pos.y, obj1.pos.z - obj2.pos.z)
	
	relPosition	
)

-- returns rotation degrees
fn getRot obj = (
	
	thetaX = obj.rotation.x_rotation
	thetaY = obj.rotation.y_rotation
	thetaZ = obj.rotation.z_rotation

	angles = #(thetaX,thetaY,thetaZ)
	
	
	angles
)

-- Compare angles, e.g. the change in angle of an object at frame X from frame 0
fn relRot obj1 obj2 = (
	
	rot1 = getRot obj1
	rot2 = getRot obj2
	
	rRot = #(rot2[1] - rot1[1], rot2[2]-rot1[2], rot2[3]-rot1[3])
	
	rRot
)

-- takes 3D vectors and subtracts them
fn subVect v1 v2 = (
	v_out = #(v2[1]-v1[1],v2[2]-v1[2],v2[3]-v1[3])
	v_out
)

-- kind of redundant, mostly for housekeeping
fn getDist obj1 obj2 = (
	
	dist = distance obj1.pos obj2.pos
	
	dist
)

-- Rounds a value to a specified number of decimal places. This gives errors in some cases
fn round val decimalPlaces = (
	
		coeff = "1"
		if decimalPlaces > 0 do (
			for i = 0 to decimalPlaces do (
				if i > 0 do (
					coeff = coeff + "0"
				)
			)
		)
		coeff = coeff as integer
		val = (floor((coeff*val)+0.5)/coeff)
)

-- Trims decimals off a floating point number by converting to string
fn trimDecimals num decimalPlaces = (
	
		strNum = num as string
		rounded = (floor num) as string
		ct = rounded.count -- the integer is the rounded count minus 2 ( or minus ".0")
		nct = strNum.count
		temp = #()
		tct = 1
	
	if num == num as integer do (
		num = num as integer
	)
	if num != num as integer do (
		for i = (ct + decimalPlaces) to nct do (
			temp[tct] = strNum[i] 
			tct += 1
		)
		for i = 0 to temp.count do (
			if (tempcount - i) > 0 do (
				trimright strNum temp[temp.count - i]
			)
		)
		num = strNum as float
	)
)

-- Makes an array of formatted time stamps for the animation.
fn timeStampArray emptyArr = (
	
	tickrate = ticksPerFrame
	ticks = #()
	
	--convert frame ticks to HH:MM:ss:ms:f
	HH = "00"		
	MM = "00"
	ss = "00"
	ms = "000000"
	
	
	-- make timestamp array
	for k = animationRange.start to animationRange.end do (
		frame = k as integer
		
		ticks[frame+1] = tickrate * k * 0.00020833333 -- seconds per tick conversion factor

		
		if ticks[frame+1] < 60 do (
			ss = (floor ticks[frame+1] as integer) as string
			ms = (ticks[frame+1] - (ss as integer)) as string
		)
		
		if ticks[frame+1] < 3600 and ticks[frame+1] >= 60 do (
			MM = (floor (ticks[frame+1] * 0.0166666666666667) as integer) as string -- seconds per min conversion factor
			ss = ((3541 - floor ticks[frame+1]) as integer) as string
			ms = (ticks[frame+1] - floor ticks[frame+1]) as string
		)
		
		if ticks[frame+1] >= 3600 do (
			HH = (floor (ticks[frame+1] * 0.000027777777) as integer) as string -- seconds per hour conversion factor
			MM = ((HH as integer) * 60 - floor (ticks[frame+1] * 0.0166666666666667) as integer) as string -- seconds per min conversion factor
			ss = ((floor ticks[frame+1] - (HH as integer) * 60 - (MM as integer) * 60) as integer) as string
			ms = (ticks[frame+1] - floor ticks[frame+1]) as string
		)
		
		if HH.count == 1 do (
			HH = "0" + HH
		)
		
		if MM.count == 1 do (
			MM = "0" + MM
		)
		
		if ss.count == 1 do (
			ss = "0" + ss
		)
		
		ms = trimleft ms "0."
		if ms.count == 0 do (
			ms = "000000"
		)
		if ms.count < 6 do (
			while ms.count < 6 do (
				ms = ms + "0"
			)
		)
		
		
		tsStream = stringstream ""
		format "%:%:%:%:%" HH MM ss ms k to:tsStream
		temp = tsStream as string
		emptyArr[frame+1] = temp
	)
	
)
-- RECORD FUNCTION


-- Record object tree, assuming the same objects are present in all frames
fn recordChildren Parent = (

	F0 = collectMyChildren Parent includeParent:true
	objcount = F0.count
	
	if F0.count == 1 do (
		messagebox "Error, object has no children"
	)
	if F0.count > 1 do (
		path = getSaveFileName caption:"Choose path" filename:"output.csv" types:"format text (*.csv)|*.csv|"
		if (getfiles path).count > 0 do (
			messagebox "Error, file already exists"
		)
		if (getfiles path).count == 0 do (
			if path != undefined do (
				file = createFile path
				format "Port \n ,time stamp," to:file
				print path
				-- get initial frame data
				sliderTime = 0


				-- Formatting loop to fill rest of column headers
				for i = 1 to objcount do (
					format "% parentRelX, % parentRelY, % parentRelZ, % parentRelDistanceChange, % parentRelThetaX, % ParentRelThetaY, %ParentRelThetaZ," \
					F0[i].name F0[i].name F0[i].name F0[i].name F0[i].name F0[i].name F0[i].name F0[i].name to:file
					format "% global X, % global Y, % global Z, % global distanceChange, % global thetaX, % global thetaY, % global thetaZ,"  \ 
					F0[i].name F0[i].name F0[i].name F0[i].name F0[i].name F0[i].name F0[i].name to:file
					
					if i == objcount do (
						format "\n" to:file
					)
				)

				ts = #()
				timeStampArray ts

				Fk = F0
				thisPosFsub1 = #(0,0,0)
				thisRelPosFsub1 = #(0,0,0)
				animate on (
					--for each frame
					for k = animationRange.start to animationRange.end do (
						frame = k as integer
						at time k (
							-- Get current frame data
							Fk = collectMyChildren Parent includeParent:true
							for i = 1 to objcount do (
								-- For parent do
								if i == 1 do (
									
									thisPos = Fk[i].pos
									thisRot = [Fk[i].rotation.x_rotation , Fk[i].rotation.y_rotation , Fk[i].rotation.z_rotation]
									
									thisRelPos = [0,0,0]
									thisRelRot = [0,0,0]
									
									if k > animationRange.start do (
										thisChangeInPos = subVect thisPosFsub1[i] thisPos 
										thisRelChangeInPos = subVect thisRelPosFsub1[i] thisRelPos
									)
									
									-- initial frame = 0 change
									if k == animationRange.start do (
										thisChangeInPos = [0,0,0]
										thisRelChangeInPos = [0,0,0]
									)
									
									thisDistanceChange = sqrt ((thisChangeInPos[1])^2 + (thisChangeInPos[2])^2 + (thisChangeInPos[3])^2)
									
									thisRelDistanceChange = 0						
									format "Frame %,%,%,%,%,%,%,%,%," \ 
									k ts[frame+1] thisRelPos[1] thisRelPos[2] thisRelPos[3] thisRelDistanceChange thisRelRot[1] thisRelRot[2] thisRelRot[3] to:file
									
									format "%,%,%,%,%,%,%," \
									thisPos[1] thisPos[2] thisPos[3] thisDistanceChange thisRot[1] thisRot[2] thisRot[3] to:file
									
								)
								-- For children of parent do
								if i > 1 do ( 
									
										-- Global position
									thisPos = Fk[i].pos
									thisRot = [Fk[i].rotation.x_rotation , Fk[i].rotation.y_rotation , Fk[i].rotation.z_rotation] 
									
									-- Position relative to parent
									in coordsys parent (
										thisRelPos = Fk[i].pos
										thisRelRot = [Fk[i].rotation.x_rotation,Fk[i].rotation.y_rotation,Fk[i].rotation.z_rotation]
									)
									
									if k > animationRange.start do (
										thisChangeInPos = subVect thisPosFsub1[i] thisPos 
										thisRelChangeInPos = subVect thisRelPosFsub1[i] thisRelPos
									)
									
									-- initial frame = 0 change
									if k == animationRange.start do (
										thisChangeInPos = subVect thisPos thisPos
										thisRelChangeInPos = subVect thisRelPos thisRelPos
									)
									
									thisDistanceChange = sqrt ((thisChangeInPos[1])^2 + (thisChangeInPos[2])^2 + (thisChangeInPos[3])^2)
									if thisChangeInPos[1]+thisChangeInPos[2]+thisChangeInPos[3] < 0 do (
										thisDistanceChange = 0 - thisDistanceChange
									)

									thisRelDistanceChange = sqrt((thisRelChangeInPos[1])^2+(thisChangeInPos[2])^2 + (thisChangeInPos[3])^2)
									
									if thisRelChangeInPos[1]+thisRelChangeInPos[2]+thisRelChangeInPos[3] < 0 do (
										thisRelDistanceChange = 0 - thisRelDistanceChange
									)
									
									format "%,%,%,%,%,%,%," \
									thisRelPos[1] thisRelPos[2] thisRelPos[3] thisRelDistanceChange thisRelRot[1] thisRelRot[2] thisRelRot[3] to:file
									
									format "%,%,%,%,%,%,%," \
									thisPos[1] thisPos[2] thisPos[3] thisDistanceChange thisRot[1] thisRot[2] thisRot[3] to:file
									
									if i == objcount do (
										format " \n " to:file
									)
								)
								thisPosFsub1[i] = thisPos
								thisRelPosFsub1[i] = thisRelPos
								
							)
						)
					)
				)

				close file
				-- Return true when completed
				true
			)
		)
	)
)

fn recordObj obj = (
	
	path = getSaveFileName caption:"Choose path" filename:"object.csv" types:"format text (*.csv)|*.csv|"
	if (getfiles path).count > 0 do (
		messagebox "Error, file already exists"
	)
	if (getfiles path).count == 0 do (
	if path != undefined do (
	print path
	file = createFile path
	format "time stamp," to:file
	-- get initial frame data
	sliderTime = 0
	F0 = obj
	
	ts = #()
	timeStampArray ts
	
	format "% parentRelX, % parentRelY, % parentRelZ, % parentRelDistanceChange, % parentRelThetaX, % parentRelThetaY, % parentRelThetaZ,% X,% Y,% Z, % distanceChange, % thetaX, % thetaY, % thetaZ \n" \
	obj.name obj.name obj.name obj.name obj.name obj.name obj.name obj.name obj.name obj.name obj.name obj.name obj.name obj.name to:file
	
	thisPosFsub1 = #(0,0,0)
	thisRelPosFsub1 = #(0,0,0)
	animate on (
		for k = animationRange.start to animationRange.end do (
			frame = k as integer
			at time k (
				thisPos = obj.pos
				thisRot = [obj.rotation.x_rotation, obj.rotation.y_rotation, obj.rotation.z_rotation]
				
				thisRelPos = #(0,0,0)
				thisRelRot = #(0,0,0)
				if obj.parent != undefined do (
					-- Position relative to parent
					parentobj = obj.parent
					in coordsys parent (
						thisRelPos = obj.pos
						thisRelRot = [obj.rotation.x_rotation,obj.rotation.y_rotation,obj.rotation.z_rotation]
					)
				)
				
				if k > animationRange.start do (
					thisChangeInPos = subVect thisPosFsub1 thisPos 
					thisRelChangeInPos = subVect thisRelPosFsub1 thisRelPos
				)
				-- initial frame = 0 change
				if k == animationRange.start do (
					thisChangeInPos = subVect thisPos thisPos
					thisRelChangeInPos = subVect thisRelPos thisRelPos
				)
				
				thisDistanceChange = sqrt ((thisChangeInPos[1])^2 + (thisChangeInPos[2])^2 + (thisChangeInPos[3])^2)
				if thisChangeInPos[1]+thisChangeInPos[2]+thisChangeInPos[3] < 0 do (
					thisDistanceChange = 0 - thisDistanceChange
				)
				thisRelDistanceChange = sqrt((thisRelChangeInPos[1])^2+(thisChangeInPos[2])^2 + (thisChangeInPos[3])^2)
				
				if thisRelChangeInPos[1]+thisRelChangeInPos[2]+thisRelChangeInPos[3] < 0 do (
					thisRelDistanceChange = 0 - thisRelDistanceChange
				)
				
				
				format "Frame %,%,%,%,%,%,%,%,%,%,%,%,%,%,%,%\n" \
					k ts[frame+1] thisRelPos[1] thisRelPos[2] thisRelPos[3] thisRelDistanceChange thisRelRot[1] thisRelRot[2] thisRelRot[3] \ 
					thisPos[1] thisPos[2] thisPos[3] thisDistanceChange thisRot[1] thisRot[2] thisRot[3] to:file
			)
		thisPosFsub1 = thisPos
		thisRelPosFsub1 = thisRelPos
		)
	)
	
	close file
	
	-- Return true when completed
	true
	)
	)
)

fn treeCSVtoObjTree Parent useParentView:false = (
	
	-- Make sure the CSV is closed in other programs
	path = getOpenFileName caption:"Choose CSV to read" types:"(*.csv)|*.csv|"
	if path != undefined then fS = openfile path
	
	PRF = #()
	
	if fS != undefined do
    (
		counter = 1
        while (not eof(fS)) do
         (
			PR = readline fS; 
			PRF[counter] = filterstring PR "," Splitemptytokens:True
			counter += 1
		 )

	
		-- For our CSV format, beginning at column 3 row 3:
			-- Objects are in the order collectMyChildren() grabs them.
			-- Column order: Frame, timestamp, relX, relY, relZ, relDist, relThetaX, relThetaY, relThetaZ, global X, global Y, global Z, distance, global thetaX, global thetaY, global thetaZ, 
		-- Just read and apply X,Y,Z, and thetaX,Y,Z to animation.

		animate on (
			for k = animationRange.start to animationRange.end do (
				at time k (
					-- Get frame data
					Fk = collectMyChildren parent includeParent:true
					-- Loop through CSV and apply data for each object at each frame
					for i = 1 to Fk.count do (
						if useParentView == false do (
							Fk[i].pos = [PRF[3+k][10+14 * (i - 1)] as float,PRF[3+k][11+14 * (i - 1)] as float,PRF[3+k][12+14 * (i - 1)] as float]
							-- in degrees!
							Fk[i].rotation.x_rotation = PRF[3+k][14+14 * (i - 1)] as float
							Fk[i].rotation.y_rotation = PRF[3+k][15+14 * (i - 1)] as float
							Fk[i].rotation.z_rotation = PRF[3+k][16+14 * (i - 1)] as float
						)
						if useParentView == true do (
							if i > 1 do ( -- skip parent
								in coordsys parent (
									Fk[i].pos = [PRF[3+k][3+14 * (i - 1)] as float,PRF[3+k][4+14 * (i - 1)] as float,PRF[3+k][5+14 * (i - 1)] as float]
									-- in degrees!
									Fk[i].rotation.x_rotation = PRF[3+k][7+14 * (i - 1)] as float
									Fk[i].rotation.y_rotation = PRF[3+k][8+14 * (i - 1)] as float
									Fk[i].rotation.z_rotation = PRF[3+k][9+14 * (i - 1)] as float
								)
							)
						)
					)
				)
			)
		)
	)	 
	
	close fS
	
)

-- Modify only a specific object from CSV of parent animation tree
-- Default uses global values
fn treeCSVtoObj Parent obj useParentView:false = (
	
	-- Find index of object in parent tree
	F0 = collectMyChildren parent includeParent:true
	index = findItem F0 obj
	
	if index != 0 do (
		-- Make sure the CSV is closed in other programs
		path = getOpenFileName caption:"Choose CSV to read" types:"(*.csv)|*.csv|"
		if path != undefined then fS = openfile path
		
		PRF = #()
		
		if fS != undefined do
		(
			counter = 1
			while (not eof(fS)) do
			 (
				PR = readline fS; 
				PRF[counter] = filterstring PR "," Splitemptytokens:True
				counter += 1
			 )

		
			-- For our CSV format, beginning at column 3 row 3:
				-- Objects are in the order collectMyChildren() grabs them.
				-- Column order: relX, relY, relZ, relDist, relThetaX, relThetaY, relThetaZ, global X, global Y, global Z, distance, global thetaX, global thetaY, global thetaZ, 
			-- Just read and apply X,Y,Z, and thetaX,Y,Z to animation.
		
			animate on (
				for k = animationRange.start to animationRange.end do (
					at time k (
						-- Get frame data
						Fk = collectMyChildren parent includeParent:true
						if useParentView == false do (
							-- Loop through CSV and apply data for each object at each frame
							Fk[index].pos = [PRF[3+k][10+14 * (index - 1)] as float,PRF[3+k][11+14 * (index - 1)] as float,PRF[3+k][12+14 * (index - 1)] as float]
							-- in degrees!
							Fk[index].rotation.x_rotation = PRF[3+k][14+14 * (index - 1)] as float
							Fk[index].rotation.y_rotation = PRF[3+k][15+14 * (index - 1)] as float
							Fk[index].rotation.z_rotation = PRF[3+k][16+14 * (index - 1)] as float
						)
						if useParentView == true do (
							if index > 1 do ( -- skip parent
								in coordsys parent (
									Fk[index].pos = [PRF[3+k][3+14 * (index - 1)] as float,PRF[3+k][4+14 * (index - 1)] as float,PRF[3+k][5+14 * (index - 1)] as float]
									-- in degrees!
									Fk[index].rotation.x_rotation = PRF[3+k][7+14 * (index - 1)] as float
									Fk[index].rotation.y_rotation = PRF[3+k][8+14 * (index - 1)] as float
									Fk[index].rotation.z_rotation = PRF[3+k][9+14 * (index - 1)] as float
								)
							)
						)
					)
				)
			)
		
		 
		
		close fS
			
		)
	)
	if index == 0 do (
		messagebox "Error: Object is not Child of Parent Selection"
	)
)

-- Modify an Object from a CSV recorded only for this object
fn CSVtoObj obj useParentView:false = (
	
	-- Make sure the CSV is closed in other programs
	path = getOpenFileName caption:"Choose CSV to read" types:"(*.csv)|*.csv|"
	if path != undefined then fS = openfile path
	
	PRF = #()
	
	if fS != undefined do
    (
		counter = 1
        while (not eof(fS)) do
         (
			PR = readline fS; 
			PRF[counter] = filterstring PR "," Splitemptytokens:True
			counter += 1
		 )

		
		-- For our CSV format, beginning at column 3 row 3:
			-- Objects are in the order collectMyChildren() grabs them.
			-- Column order: relX, relY, relZ, relDist, relThetaX, relThetaY, relThetaZ, global X, global Y, global Z, distance, global thetaX, global thetaY, global thetaZ, 
		-- Just read and apply X,Y,Z, and thetaX,Y,Z to animation.

		animate on (
			for k = animationRange.start to animationRange.end do (
				at time k (
					if useParentView == false do (
					-- Loop through CSV and apply data for each object at each frame
					obj.pos = [PRF[3+k][10] as float,PRF[3+k][11] as float,PRF[3+k][12] as float]
					-- in degrees!
					obj.rotation.x_rotation = PRF[3+k][14] as float
					obj.rotation.y_rotation = PRF[3+k][15] as float
					obj.rotation.z_rotation = PRF[3+k][16] as float
					)
					if useParentView == true do (
						in coordsys parent (
							-- Loop through CSV and apply data for each object at each frame
							obj.pos = [PRF[3+k][3] as float,PRF[3+k][4] as float,PRF[3+k][5] as float]
							-- in degrees!
							obj.rotation.x_rotation = PRF[3+k][7] as float
							obj.rotation.y_rotation = PRF[3+k][8] as float
							obj.rotation.z_rotation = PRF[3+k][9] as float
						)
					)
				)
			)
		)
		close fS
	)
)






-- GUI CONTROL FUNCTIONS --
firstobj = #() 


rollout treeCSVtoObjAdditionalGUI "Select Child Object" (
	label l4 "Select child object."
	button okay "Next" width:190
	label l3 "" 
	on okay pressed do (
		if $ == undefined do (
			messagebox "Error: No object selected"
		)
		if $ != undefined do (
			if CSVGUI.types.state == 1 do (
				treeCSVtoObj firstObj $
			)
			if CSVGUI.types.state == 2 do (
				treeCSVtoObj firstObj $ useParentView:true
			)
			l3.text = "Applied to " + $.name
		)
	)
)
-- Currently for reading and writing CSVs with object global and parent orientation animaton data.
rollout CSVGUI "Record/Apply CSV Animation Data" (
	
	label title "Record/Apply CSV Animation Data"
	button recordObject "Record Selected Object" offset:[0,10]
	checkbox inclChildren "Include Children?" align:#right
	label l2 "" align:#right
	label line1 "_______________________________" offset:[0,5]
	
	button readObjCSV "Apply CSV for Object" offset:[0.0, 10]

	radiobuttons types labels:#("Globals","ParentView")
	radiobuttons functions labels:#("Single Object","Object plus Children","Child Object from Parent CSV")
	label note "Note: This must have been recorded "
	label note2 "with the above function."
	label l1 "" align:#right
	on recordObject pressed do (
		if $ == undefined do (
			messagebox "Error: No object selected"
		)
		if $ != undefined do (
			if inclChildren.checked == true do (
				recordChildren $
				l2.text = "Completed"
			)
			if inclChildren.checked == false do (
				recordObj $
				l2.text = "Completed"
			)
		)
	)

	on readObjCSV pressed do (
		if $ == undefined do (
			messagebox "Error: No object selected"
		)
		if $ != undefined do (
			if CSVGUI.types.state == 1 do (
				if CSVGUI.functions.state == 1 do (
					CSVtoObj $
					l1.text = "Completed"
				)
				if CSVGUI.functions.state == 2 do (
					treeCSVtoObjTree $
					l1.text = "Completed"
				)
				if CSVGUI.functions.state == 3 do (
					firstObj = $
					createDialog treeCSVtoObjAdditionalGUI 200 75
				)
			)
			if CSVGUI.types.state == 2 do (
				if CSVGUI.functions.state == 1 do (
					CSVtoObj $ useParentView:true
					l1.text = "Completed"
				)
				if CSVGUI.functions.state == 2 do (
					treeCSVtoObjTree $ useParentView:true
					l1.text = "Completed"
				)
				if CSVGUI.functions.state == 3 do (
					firstObj = $
					createDialog treeCSVtoObjAdditionalGUI 200 75
				)
			)
		)
	)
	
)

createDialog CSVGUI 200 350



