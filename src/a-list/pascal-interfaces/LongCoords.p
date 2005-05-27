UNIT LongCoords;

{ Pascal interface to the A List: }
{ Long Coordinates }

{ Borrowed from the WASTE library. }
{ Copyright © 1993-1996 Marco Piovanelli }
{ All Rights Reserved }

INTERFACE
	USES
		Types;

	TYPE

{ long coordinates types }

		LongPt = RECORD
				CASE INTEGER OF
					0: (
							v: LONGINT;
							h: LONGINT;
					);
					1: (
{$IFC NOT UNDEFINED THINK_PASCAL}
							vh: ARRAY[VHSelect] OF LONGINT;
{$ELSEC}
							vh: ARRAY[0..1] OF LONGINT;
{$ENDC}
					);
			END;  { LongPt }

		LongRect = RECORD
				CASE INTEGER OF
					0: (
							top: LONGINT;
							left: LONGINT;
							bottom: LONGINT;
							right: LONGINT;
					);
					1: (
							topLeft: LongPt;
							botRight: LongPt;
					);
			END;  { LongRect }

IMPLEMENTATION
END.