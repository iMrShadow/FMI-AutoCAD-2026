lego_brick_dialog : dialog {
  label       = "LEGO Brick Generator";
  width       = 60;
  fixed_width = true;

  : row {
    : column {
      : boxed_column {
        label = "Dimensions";
        : row {
          : text { label = "Width (studs)"; }
          : edit_box {
            key        = "width";
            value      = "2";
            edit_width = 2;
            alignment  = centered;
          }
          : text { label = "Depth (studs)"; }
          : edit_box {
            key        = "depth";
            value      = "4";
            edit_width = 2;
            alignment  = centered;
          }
        }

      : radio_row {
        key    = "htype";
        : radio_button { key = "rb_brick"; label = "Brick (full)";  value = "1"; }
        : radio_button { key = "rb_plate"; label = "Plate (1/3)";   value = "0"; }
      }
    }

    : boxed_column {
      label = "Options";
      : popup_list {
          label = "Select color:";
          key   = "color";
          list  = "";
          value = "0";
          width = 16;
        }

      : row {
        : toggle {
          key   = "opt_ridges";
          label = "Ridges";
          value = "1";
      }
      : toggle {
        key   = "opt_studs";
        label = "Studs on top";
        value = "1";
      }
    }
    : row {
      : toggle {
        key   = "opt_tubes";
        label = "Tubes / Dimples";
        value = "1";
      }
      : toggle {
        key   = "opt_ribs";
        label = "Interior ribs";
        value = "1";
      }
        }
      }   
    }

    : column {
      : boxed_column {
        label = "Preview";
        alignment = centered;
        : column {
            : image {
              key       = "preview";
              width     = 44;
              height    = 16;
              color     = 0;
              alignment = centered;
            }
        }
      }
    }
  }

  : text {
    key       = "status";
    label     = "Ready.";
    alignment = left;
    width     = 50;
  }

  spacer_1;

  ok_cancel;
}