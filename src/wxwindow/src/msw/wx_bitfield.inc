
/* Included by wx_gdi.cxx */

/*

(require (lib "list.ss"))

(define ranges
  '((0  #x0040  #x005A)
    (0  #x0060  #x007A)
    (1  #x00A0  #x00FF)
    (2  #x0100  #x017F)
    (3  #x0180  #x024F)
    (4  #x0250  #x02AF)
    (5  #x02B0  #x02FF)
    (6  #x0300  #x036F)
    (7  #x0370  #x03FF)
    (9  #x0400  #x04FF)
    (9  #x0500  #x052F)
    (10  #x0530  #x058F)
    (11  #x0590  #x05FF)
    (13  #x0600  #x06FF)
    (15  #x0900  #x097F)
    (16  #x0980  #x09FF)
    (17  #x0A00  #x0A7F)
    (18  #x0A80  #x0AFF)
    (19  #x0B00  #x0B7F)
    (20  #x0B80  #x0BFF)
    (21  #x0C00  #x0C7F)
    (22  #x0C80  #x0CFF)
    (23  #x0D00  #x0D7F)
    (24  #x0E00  #x0E7F)
    (25  #x0E80  #x0EFF)
    (26  #x10A0  #x10FF)
    (28  #x1100  #x11FF)
    (29  #x1E00  #x1EFF)
    (30  #x1F00  #x1FFF)
    (31  #x2000  #x206F)
    (32  #x2070  #x209F)
    (33  #x20A0  #x20CF)
    (34  #x20D0  #x20FF)
    (35  #x2100  #x214F)
    (36  #x2150  #x218F)
    (37  #x2190  #x21FF)
    (37  #x27F0  #x27FF)
    (37  #x2900  #x297F)
    (38  #x2200  #x22FF)
    (38  #x2A00  #x2AFF)
    (38  #x27C0  #x27EF)
    (38  #x2980  #x29FF)
    (39  #x2300  #x23FF)
    (40  #x2400  #x243F)
    (41  #x2440  #x245F)
    (42  #x2460  #x24FF)
    (43  #x2500  #x257F)
    (44  #x2580  #x259F)
    (45  #x25A0  #x25FF)
    (46  #x2600  #x26FF)
    (47  #x2700  #x27BF)
    (48  #x3000  #x303F)
    (49  #x3040  #x309F)
    (50  #x30A0  #x30FF)
    (50  #x31F0  #x31FF)
    (51  #x3100  #x312F)
    (51  #x31A0  #x31Bf)
    (52  #x3130  #x318F)
    (54  #x3200  #x32FF)
    (55  #x3300  #x33FF)
    (56  #xAC00  #xD7A3)
    (57  #xD800  #xDFFF)
    (59  #x4E00  #x9FFF)
    (59  #x2E80  #x2EFF)
    (59  #x2F00  #x2FDF)
    (59  #x2FF0  #x2FFF)
    (59  #x3190  #x319F)
    (59  #x3400  #x4DBF)
    (59  #x20000  #x2A6DF)
    (60  #xE000  #xF8FF)
    (61  #xF900  #xFAFF)
    (61  #x2F800  #x2FA1F)
    (62  #xFB00  #xFB4F)
    (63  #xFB50  #xFDFF)
    (64  #xFE20  #xFE2F)
    (65  #xFE30  #xFE4F)
    (66  #xFE50  #xFE6F)
    (67  #xFE70  #xFEFE)
    (68  #xFF00  #xFFEF)
    (69  #xFFF0  #xFFFF)
    (70  #x0F00  #x0FFF)
    (71  #x0700  #x074F)
    (72  #x0780  #x07BF)
    (73  #x0D80  #x0DFF)
    (74  #x1000  #x109F)
    (75  #x1200  #x12BF)
    (76  #x13A0  #x13FF)
    (77  #x1400  #x167F)
    (78  #x1680  #x169F)
    (79  #x16A0  #x16FF)
    (80  #x1780  #x17FF)
    (80  #x19E0  #x19FF)
    (81  #x1800  #x18AF)
    (82  #x2800  #x28FF)
    (83  #xA000  #xA48F)
    (83  #xA480  #xA4CF)
    (84  #x1700  #x171F)
    (84  #x1720  #x173F)
    (84  #x1740  #x175F)
    (84  #x1760  #x177F)
    (85  #x10300  #x1032F)
    (86  #x10330  #x1034F)
    (87  #x10440  #x1044F)
    (88  #x1D000  #x1D0FF)
    (88  #x1D100  #x1D1FF)
    (89  #x1D400  #x1D7FF)
    (90  #xFFF80  #xFFFFF)
    (90  #x10FF80  #x10FFFF)))

(define sorted (quicksort ranges (lambda (a b) (< (cadr a) (cadr b)))))

(define (first-n l n)
  (if (zero? n)
      null
      (cons (car l) (first-n (cdr l) (sub1 n)))))

(define (output l indent)
  (if (= 1 (length l))
      (printf "~aif ((c >= 0x~x) && (c <= 0x~x)) return ~a;\n" 
	      indent
	      (cadr (car l))
	      (caddr (car l))
	      (car (car l)))
      (let ([n (length l)])
	(let ([a (first-n l (quotient n 2))]
	      [b (list-tail l (quotient n 2))])
	  (printf "~aif (c < 0x~x) {\n" indent (cadr (car b)))
	  (output a (string-append  indent "  "))
	  (printf "~a} else {\n"  indent)
	  (output b (string-append  indent "  "))
	  (printf "~a}\n" indent)))))
	  
(output sorted "  ")
(printf "  return 127;")

*/

int get_bitfield(int c)
{
  if (c < 0x2300) {
    if (c < 0xe80) {
      if (c < 0x600) {
        if (c < 0x2b0) {
          if (c < 0x100) {
            if (c < 0x60) {
              if ((c >= 0x40) && (c <= 0x5a)) return 0;
            } else {
              if (c < 0xa0) {
                if ((c >= 0x60) && (c <= 0x7a)) return 0;
              } else {
                if ((c >= 0xa0) && (c <= 0xff)) return 1;
              }
            }
          } else {
            if (c < 0x180) {
              if ((c >= 0x100) && (c <= 0x17f)) return 2;
            } else {
              if (c < 0x250) {
                if ((c >= 0x180) && (c <= 0x24f)) return 3;
              } else {
                if ((c >= 0x250) && (c <= 0x2af)) return 4;
              }
            }
          }
        } else {
          if (c < 0x400) {
            if (c < 0x300) {
              if ((c >= 0x2b0) && (c <= 0x2ff)) return 5;
            } else {
              if (c < 0x370) {
                if ((c >= 0x300) && (c <= 0x36f)) return 6;
              } else {
                if ((c >= 0x370) && (c <= 0x3ff)) return 7;
              }
            }
          } else {
            if (c < 0x530) {
              if (c < 0x500) {
                if ((c >= 0x400) && (c <= 0x4ff)) return 9;
              } else {
                if ((c >= 0x500) && (c <= 0x52f)) return 9;
              }
            } else {
              if (c < 0x590) {
                if ((c >= 0x530) && (c <= 0x58f)) return 10;
              } else {
                if ((c >= 0x590) && (c <= 0x5ff)) return 11;
              }
            }
          }
        }
      } else {
        if (c < 0xb00) {
          if (c < 0x900) {
            if (c < 0x700) {
              if ((c >= 0x600) && (c <= 0x6ff)) return 13;
            } else {
              if (c < 0x780) {
                if ((c >= 0x700) && (c <= 0x74f)) return 71;
              } else {
                if ((c >= 0x780) && (c <= 0x7bf)) return 72;
              }
            }
          } else {
            if (c < 0xa00) {
              if (c < 0x980) {
                if ((c >= 0x900) && (c <= 0x97f)) return 15;
              } else {
                if ((c >= 0x980) && (c <= 0x9ff)) return 16;
              }
            } else {
              if (c < 0xa80) {
                if ((c >= 0xa00) && (c <= 0xa7f)) return 17;
              } else {
                if ((c >= 0xa80) && (c <= 0xaff)) return 18;
              }
            }
          }
        } else {
          if (c < 0xc80) {
            if (c < 0xb80) {
              if ((c >= 0xb00) && (c <= 0xb7f)) return 19;
            } else {
              if (c < 0xc00) {
                if ((c >= 0xb80) && (c <= 0xbff)) return 20;
              } else {
                if ((c >= 0xc00) && (c <= 0xc7f)) return 21;
              }
            }
          } else {
            if (c < 0xd80) {
              if (c < 0xd00) {
                if ((c >= 0xc80) && (c <= 0xcff)) return 22;
              } else {
                if ((c >= 0xd00) && (c <= 0xd7f)) return 23;
              }
            } else {
              if (c < 0xe00) {
                if ((c >= 0xd80) && (c <= 0xdff)) return 73;
              } else {
                if ((c >= 0xe00) && (c <= 0xe7f)) return 24;
              }
            }
          }
        }
      }
    } else {
      if (c < 0x1760) {
        if (c < 0x13a0) {
          if (c < 0x10a0) {
            if (c < 0xf00) {
              if ((c >= 0xe80) && (c <= 0xeff)) return 25;
            } else {
              if (c < 0x1000) {
                if ((c >= 0xf00) && (c <= 0xfff)) return 70;
              } else {
                if ((c >= 0x1000) && (c <= 0x109f)) return 74;
              }
            }
          } else {
            if (c < 0x1100) {
              if ((c >= 0x10a0) && (c <= 0x10ff)) return 26;
            } else {
              if (c < 0x1200) {
                if ((c >= 0x1100) && (c <= 0x11ff)) return 28;
              } else {
                if ((c >= 0x1200) && (c <= 0x12bf)) return 75;
              }
            }
          }
        } else {
          if (c < 0x16a0) {
            if (c < 0x1400) {
              if ((c >= 0x13a0) && (c <= 0x13ff)) return 76;
            } else {
              if (c < 0x1680) {
                if ((c >= 0x1400) && (c <= 0x167f)) return 77;
              } else {
                if ((c >= 0x1680) && (c <= 0x169f)) return 78;
              }
            }
          } else {
            if (c < 0x1720) {
              if (c < 0x1700) {
                if ((c >= 0x16a0) && (c <= 0x16ff)) return 79;
              } else {
                if ((c >= 0x1700) && (c <= 0x171f)) return 84;
              }
            } else {
              if (c < 0x1740) {
                if ((c >= 0x1720) && (c <= 0x173f)) return 84;
              } else {
                if ((c >= 0x1740) && (c <= 0x175f)) return 84;
              }
            }
          }
        }
      } else {
        if (c < 0x2070) {
          if (c < 0x19e0) {
            if (c < 0x1780) {
              if ((c >= 0x1760) && (c <= 0x177f)) return 84;
            } else {
              if (c < 0x1800) {
                if ((c >= 0x1780) && (c <= 0x17ff)) return 80;
              } else {
                if ((c >= 0x1800) && (c <= 0x18af)) return 81;
              }
            }
          } else {
            if (c < 0x1f00) {
              if (c < 0x1e00) {
                if ((c >= 0x19e0) && (c <= 0x19ff)) return 80;
              } else {
                if ((c >= 0x1e00) && (c <= 0x1eff)) return 29;
              }
            } else {
              if (c < 0x2000) {
                if ((c >= 0x1f00) && (c <= 0x1fff)) return 30;
              } else {
                if ((c >= 0x2000) && (c <= 0x206f)) return 31;
              }
            }
          }
        } else {
          if (c < 0x2100) {
            if (c < 0x20a0) {
              if ((c >= 0x2070) && (c <= 0x209f)) return 32;
            } else {
              if (c < 0x20d0) {
                if ((c >= 0x20a0) && (c <= 0x20cf)) return 33;
              } else {
                if ((c >= 0x20d0) && (c <= 0x20ff)) return 34;
              }
            }
          } else {
            if (c < 0x2190) {
              if (c < 0x2150) {
                if ((c >= 0x2100) && (c <= 0x214f)) return 35;
              } else {
                if ((c >= 0x2150) && (c <= 0x218f)) return 36;
              }
            } else {
              if (c < 0x2200) {
                if ((c >= 0x2190) && (c <= 0x21ff)) return 37;
              } else {
                if ((c >= 0x2200) && (c <= 0x22ff)) return 38;
              }
            }
          }
        }
      }
    }
  } else {
    if (c < 0x3300) {
      if (c < 0x2980) {
        if (c < 0x25a0) {
          if (c < 0x2460) {
            if (c < 0x2400) {
              if ((c >= 0x2300) && (c <= 0x23ff)) return 39;
            } else {
              if (c < 0x2440) {
                if ((c >= 0x2400) && (c <= 0x243f)) return 40;
              } else {
                if ((c >= 0x2440) && (c <= 0x245f)) return 41;
              }
            }
          } else {
            if (c < 0x2500) {
              if ((c >= 0x2460) && (c <= 0x24ff)) return 42;
            } else {
              if (c < 0x2580) {
                if ((c >= 0x2500) && (c <= 0x257f)) return 43;
              } else {
                if ((c >= 0x2580) && (c <= 0x259f)) return 44;
              }
            }
          }
        } else {
          if (c < 0x27c0) {
            if (c < 0x2600) {
              if ((c >= 0x25a0) && (c <= 0x25ff)) return 45;
            } else {
              if (c < 0x2700) {
                if ((c >= 0x2600) && (c <= 0x26ff)) return 46;
              } else {
                if ((c >= 0x2700) && (c <= 0x27bf)) return 47;
              }
            }
          } else {
            if (c < 0x2800) {
              if (c < 0x27f0) {
                if ((c >= 0x27c0) && (c <= 0x27ef)) return 38;
              } else {
                if ((c >= 0x27f0) && (c <= 0x27ff)) return 37;
              }
            } else {
              if (c < 0x2900) {
                if ((c >= 0x2800) && (c <= 0x28ff)) return 82;
              } else {
                if ((c >= 0x2900) && (c <= 0x297f)) return 37;
              }
            }
          }
        }
      } else {
        if (c < 0x30a0) {
          if (c < 0x2f00) {
            if (c < 0x2a00) {
              if ((c >= 0x2980) && (c <= 0x29ff)) return 38;
            } else {
              if (c < 0x2e80) {
                if ((c >= 0x2a00) && (c <= 0x2aff)) return 38;
              } else {
                if ((c >= 0x2e80) && (c <= 0x2eff)) return 59;
              }
            }
          } else {
            if (c < 0x3000) {
              if (c < 0x2ff0) {
                if ((c >= 0x2f00) && (c <= 0x2fdf)) return 59;
              } else {
                if ((c >= 0x2ff0) && (c <= 0x2fff)) return 59;
              }
            } else {
              if (c < 0x3040) {
                if ((c >= 0x3000) && (c <= 0x303f)) return 48;
              } else {
                if ((c >= 0x3040) && (c <= 0x309f)) return 49;
              }
            }
          }
        } else {
          if (c < 0x3190) {
            if (c < 0x3100) {
              if ((c >= 0x30a0) && (c <= 0x30ff)) return 50;
            } else {
              if (c < 0x3130) {
                if ((c >= 0x3100) && (c <= 0x312f)) return 51;
              } else {
                if ((c >= 0x3130) && (c <= 0x318f)) return 52;
              }
            }
          } else {
            if (c < 0x31f0) {
              if (c < 0x31a0) {
                if ((c >= 0x3190) && (c <= 0x319f)) return 59;
              } else {
                if ((c >= 0x31a0) && (c <= 0x31bf)) return 51;
              }
            } else {
              if (c < 0x3200) {
                if ((c >= 0x31f0) && (c <= 0x31ff)) return 50;
              } else {
                if ((c >= 0x3200) && (c <= 0x32ff)) return 54;
              }
            }
          }
        }
      }
    } else {
      if (c < 0xfe50) {
        if (c < 0xd800) {
          if (c < 0xa000) {
            if (c < 0x3400) {
              if ((c >= 0x3300) && (c <= 0x33ff)) return 55;
            } else {
              if (c < 0x4e00) {
                if ((c >= 0x3400) && (c <= 0x4dbf)) return 59;
              } else {
                if ((c >= 0x4e00) && (c <= 0x9fff)) return 59;
              }
            }
          } else {
            if (c < 0xa480) {
              if ((c >= 0xa000) && (c <= 0xa48f)) return 83;
            } else {
              if (c < 0xac00) {
                if ((c >= 0xa480) && (c <= 0xa4cf)) return 83;
              } else {
                if ((c >= 0xac00) && (c <= 0xd7a3)) return 56;
              }
            }
          }
        } else {
          if (c < 0xfb00) {
            if (c < 0xe000) {
              if ((c >= 0xd800) && (c <= 0xdfff)) return 57;
            } else {
              if (c < 0xf900) {
                if ((c >= 0xe000) && (c <= 0xf8ff)) return 60;
              } else {
                if ((c >= 0xf900) && (c <= 0xfaff)) return 61;
              }
            }
          } else {
            if (c < 0xfe20) {
              if (c < 0xfb50) {
                if ((c >= 0xfb00) && (c <= 0xfb4f)) return 62;
              } else {
                if ((c >= 0xfb50) && (c <= 0xfdff)) return 63;
              }
            } else {
              if (c < 0xfe30) {
                if ((c >= 0xfe20) && (c <= 0xfe2f)) return 64;
              } else {
                if ((c >= 0xfe30) && (c <= 0xfe4f)) return 65;
              }
            }
          }
        }
      } else {
        if (c < 0x1d000) {
          if (c < 0xfff0) {
            if (c < 0xfe70) {
              if ((c >= 0xfe50) && (c <= 0xfe6f)) return 66;
            } else {
              if (c < 0xff00) {
                if ((c >= 0xfe70) && (c <= 0xfefe)) return 67;
              } else {
                if ((c >= 0xff00) && (c <= 0xffef)) return 68;
              }
            }
          } else {
            if (c < 0x10330) {
              if (c < 0x10300) {
                if ((c >= 0xfff0) && (c <= 0xffff)) return 69;
              } else {
                if ((c >= 0x10300) && (c <= 0x1032f)) return 85;
              }
            } else {
              if (c < 0x10440) {
                if ((c >= 0x10330) && (c <= 0x1034f)) return 86;
              } else {
                if ((c >= 0x10440) && (c <= 0x1044f)) return 87;
              }
            }
          }
        } else {
          if (c < 0x20000) {
            if (c < 0x1d100) {
              if ((c >= 0x1d000) && (c <= 0x1d0ff)) return 88;
            } else {
              if (c < 0x1d400) {
                if ((c >= 0x1d100) && (c <= 0x1d1ff)) return 88;
              } else {
                if ((c >= 0x1d400) && (c <= 0x1d7ff)) return 89;
              }
            }
          } else {
            if (c < 0xfff80) {
              if (c < 0x2f800) {
                if ((c >= 0x20000) && (c <= 0x2a6df)) return 59;
              } else {
                if ((c >= 0x2f800) && (c <= 0x2fa1f)) return 61;
              }
            } else {
              if (c < 0x10ff80) {
                if ((c >= 0xfff80) && (c <= 0xfffff)) return 90;
              } else {
                if ((c >= 0x10ff80) && (c <= 0x10ffff)) return 90;
              }
            }
          }
        }
      }
    }
  }
  return 127;
}
