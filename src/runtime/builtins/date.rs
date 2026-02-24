use crate::runtime::{Heap, Value};
use std::time::{SystemTime, UNIX_EPOCH};

use super::{number_to_value, to_number};

pub fn create(args: &[Value], heap: &mut Heap) -> Value {
    let ms = match args.first() {
        None => {
            SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .map(|d| d.as_millis() as f64)
                .unwrap_or(0.0)
        }
        Some(Value::Int(n)) => *n as f64,
        Some(Value::Number(n)) => *n,
        Some(v) => to_number(v),
    };
    let id = heap.alloc_date(ms);
    Value::Date(id)
}

pub fn now(_args: &[Value], _heap: &mut Heap) -> Value {
    let ms = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_millis() as f64)
        .unwrap_or(0.0);
    number_to_value(ms)
}

pub fn get_time(args: &[Value], heap: &mut Heap) -> Value {
    let id = match args.first().and_then(Value::as_date_id) {
        Some(i) => i,
        None => return Value::Number(f64::NAN),
    };
    Value::Number(heap.date_timestamp(id))
}

fn format_date(ms: f64) -> String {
    if !ms.is_finite() {
        return "Invalid Date".to_string();
    }
    let secs = (ms / 1000.0) as i64;
    let millis = (ms % 1000.0) as i32;
    let days = secs / 86400;
    let t = secs % 86400;
    let h = t / 3600;
    let m = (t % 3600) / 60;
    let s = t % 60;
    let (y, mo, d) = days_to_ymd(days);
    format!(
        "{} {:02} {} {:04} {:02}:{:02}:{:02}.{:03} GMT",
        weekday_name((days + 4) % 7),
        d,
        month_name(mo),
        y,
        h,
        m,
        s,
        millis.abs()
    )
}

fn days_to_ymd(days: i64) -> (i32, i32, i32) {
    let z = days + 719468;
    let era = (if z >= 0 { z } else { z - 146096 }) / 146097;
    let doe = z - era * 146097;
    let yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
    let y = (yoe + era * 400) as i32;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = (doy - (153 * mp + 2) / 5 + 1) as i32;
    let mo = (mp + (if mp < 10 { 3 } else { -9 })) as i32;
    let y = y + (if mo <= 2 { 1 } else { 0 });
    (y, mo, d)
}

fn weekday_name(n: i64) -> &'static str {
    match n.rem_euclid(7) {
        0 => "Sun",
        1 => "Mon",
        2 => "Tue",
        3 => "Wed",
        4 => "Thu",
        5 => "Fri",
        6 => "Sat",
        _ => "Sun",
    }
}

fn month_name(mo: i32) -> &'static str {
    match mo {
        1 => "Jan",
        2 => "Feb",
        3 => "Mar",
        4 => "Apr",
        5 => "May",
        6 => "Jun",
        7 => "Jul",
        8 => "Aug",
        9 => "Sep",
        10 => "Oct",
        11 => "Nov",
        12 => "Dec",
        _ => "Jan",
    }
}

pub fn to_string(args: &[Value], heap: &mut Heap) -> Value {
    let id = match args.first().and_then(Value::as_date_id) {
        Some(i) => i,
        None => return Value::String("Invalid Date".to_string()),
    };
    let ms = heap.date_timestamp(id);
    Value::String(format_date(ms))
}

pub fn to_iso_string(args: &[Value], heap: &mut Heap) -> Value {
    let id = match args.first().and_then(Value::as_date_id) {
        Some(i) => i,
        None => return Value::String("Invalid Date".to_string()),
    };
    let ms = heap.date_timestamp(id);
    if !ms.is_finite() {
        return Value::String("Invalid Date".to_string());
    }
    let secs = (ms / 1000.0) as i64;
    let millis = (ms % 1000.0) as i32;
    let t = secs % 86400;
    let h = t / 3600;
    let m = (t % 3600) / 60;
    let s = t % 60;
    let days = secs / 86400;
    let (y, mo, d) = days_to_ymd(days);
    Value::String(format!(
        "{:04}-{:02}-{:02}T{:02}:{:02}:{:02}.{:03}Z",
        y,
        mo,
        d,
        h,
        m,
        s,
        millis.abs()
    ))
}
