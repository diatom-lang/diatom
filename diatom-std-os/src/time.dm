import {now, show_date_time, duration, show_duration} from std.os.time.util

DateTime = {}

def DateTime.now =
    {time = now()} <- DateTime
end

def DateTime.show self = 
    show_date_time(self.time)
end

Duration = {}

def Duration.new d1 d2 = 
    {duration = duration(d1.time, d2.time)} <- Duration
end

def Duration.show self = 
    show_duration(self.duration)
end

{  DateTime = DateTime, Duration = Duration }
