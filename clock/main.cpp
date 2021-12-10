#include <map>
#include <string>
#include <cstring>

#include <String.h>
#include <File.h>
#include <Hashmap.h>
#include <Variant.h>

struct Time
{
    unsigned hour;
    unsigned minute;
    
    static Time from_string(StringView const& string)
    {
        auto hour_and_minute_string = string.split(':');
        Time t;
        t.hour = atoi(hour_and_minute_string[0].non_null_terminated_buffer());
        t.minute = atoi(hour_and_minute_string[1].non_null_terminated_buffer());
        return t;
    }
    
    Time operator+(Time const& other) const
    {
        Time new_time(hour + other.hour, minute + other.minute);
        if (new_time.minute > 59)
        {
            new_time.hour++;
            new_time.minute %= 60;
        }
        if (new_time.hour > 23)
        {
            new_time.hour %= 24;
        }
        return new_time;
    }
    
    bool operator<(Time const& other)
    {
        return hour < other.hour || ((hour == other.hour) && minute < other.minute);
    }
    
    Time(unsigned hours, unsigned minutes) : hour(hours), minute(minutes) {}

private:
    Time() = default;
    
};

struct ClockObject
{
    bool has_num;
    ssize_t num;
    String str;
    
    ClockObject() : has_num(true), num(), str() {}
    ClockObject(String const& op) : has_num(false), num(), str(op) {}
    ClockObject(ssize_t const& op) : has_num(true), num(op), str() {}
};

struct StackFrame
{
    std::map<String, ClockObject> objects;
    size_t return_to;
    std::map<String, ClockObject> backpack;
};

ssize_t to_num(String const& s)
{
    errno = 0;
    char* endptr {};
    ssize_t num = strtol(s.null_terminated_characters(), &endptr, 10);
    if (errno != 0 || endptr == s.null_terminated_characters())
        exit(-1);
    return num;
}

ssize_t as_num(ClockObject const& o)
{
    if (o.has_num)
        return o.num;
    else
        return to_num(o.str);
}

int interpret(StringView const& source)
{
    auto lines = source.split('\n');
    Vector<StackFrame> frames;
    frames.append({ {}, 0, {}});
    size_t instruction_pointer = lines.find([](StringView const& s) { return s.ends_with("I wake up"); }).index();
    while (instruction_pointer < lines.size())
    {
        // printf("--%s\n", String(lines.at(instruction_pointer)).null_terminated_characters());
        if (lines[instruction_pointer].substring_view(6).starts_with("I wake up"))
        {
            instruction_pointer++;
            continue;
        }
        auto line = String(lines[instruction_pointer].substring_view(sizeof("hh:mm")));
        if (line.starts_with("It is!"))
            line = line.substring(sizeof("It is!"));
        else if (line.starts_with("I"))
            line = line.substring(sizeof("I"));
        else if (line.starts_with("Otherwise"))
            line = line.substring(sizeof("Otherwise"));
        
        
        //start:
        if (line.starts_with("wonder if"))
        {
            line = line.substring(sizeof("wonder if"));
            auto identifier = line.substring(line.begin(), line.find(" "));
            line = line.substring(++line.find(" "));
            if (frames.last().objects.contains(identifier))
            {
                ssize_t op1 = as_num(frames.last().objects.at(identifier));
                if (line.starts_with("is greater or equal to"))
                {
                    line = line.substring(sizeof("is greater or equal to"));
                    identifier = line;
                    ssize_t op2;
                    if (!frames.last().objects.contains(identifier))
                        op2 = to_num(identifier);
                    else
                        op2 = as_num(frames.last().objects.at(identifier));
                    
                    if (op1 >= op2)
                        instruction_pointer+=1;
                    else
                        instruction_pointer+=2;
                    continue;
                }
                else if (line.starts_with("is"))
                {
                    line = line.substring(sizeof("is"));
                    identifier = line;
                    ssize_t op2;
                    if (!frames.last().objects.contains(identifier))
                        op2 = to_num(identifier);
                    else
                        op2 = as_num(frames.last().objects.at(identifier));
                    
                    if (op1 == op2)
                        instruction_pointer+=1;
                    else
                        instruction_pointer+=2;
                    continue;
                } else
                {
                    exit(-1);
                }
            }
        }
        else if (line.starts_with("go to sleep"))
            exit(0);
        else if (line.starts_with("realize it's"))
        {
            line = line.substring(sizeof("realize it's"));
            auto time = line.substring(0, sizeof("hh:mm")-1);
            instruction_pointer = lines.find([&](StringView const& s) { return s.starts_with(time); }).index();
            continue;
        }
        else if (line.starts_with("write"))
        {
            line = line.substring(sizeof("write")+1);
            auto new_obj_name = line.substring(line.begin(), line.find("\""));
            line = line.substring(new_obj_name.length());
            frames.last().objects.insert_or_assign(new_obj_name, ClockObject{});
            instruction_pointer++;
            continue;
        }
        else if (line.starts_with("say"))
        {
            line = line.substring(sizeof("say"));
            while (!line.is_empty())
            {
                if (line.starts_with("\""))
                {
                    auto it_start = line.begin();
                    it_start++++;
                    auto end = line.to_view().substring_view(2).find("\"");
                    auto literal = line.substring(it_start, end);
                    line = line.substring(literal.length()+3);
                    printf("%s", literal.null_terminated_characters());
                }
                else
                {
                    auto identifier = line.substring(line.begin(), line.find(" "));
                    line = line.substring(min(line.length(), identifier.length()+1));
                    //ssize_t value;
                    if (!frames.last().objects.contains(identifier))
                        exit(-1);
                    
                    if (frames.last().objects.at(identifier).has_num)
                        printf("%ld", frames.last().objects.at(identifier).num);
                    else
                        printf("%s", frames.last().objects.at(identifier).str.null_terminated_characters());
                }
            }
            printf("\n");
            instruction_pointer++;
            continue;
        }
        else if (line.starts_with("go to"))
        {
            line = line.substring(sizeof("go to"));
            StackFrame frame {{}, instruction_pointer+1, frames.last().backpack};
            frames.append(move(frame));
            instruction_pointer = lines.find([&](StringView const& s) { return s.substring_view(3).starts_with(line); }).index();
            continue;
        }
        else if (line.starts_with("return from"))
        {
            line = line.substring(sizeof("return from"));
            frames[frames.size()-2].backpack = frames.last().backpack;
            instruction_pointer = frames.last().return_to;
            frames.take_last();
            continue;
        }
        else if (line.starts_with("open my backpack, read"))
        {
            line = line.substring(sizeof("open my backpack, read"));
            auto opsrc = line.substring(line.begin(), line.find(","));
            line = line.substring(opsrc.length()+2);
            if (!line.starts_with("and write the value in"))
                exit(-1);
            line = line.substring(sizeof("and write the value in"));
            auto opdst = line;
            
            if (!frames.last().backpack.contains(opsrc) || !frames.last().objects.contains(opdst))
                exit(-1);
            
            frames.last().objects.at(opdst) = frames.last().backpack.at(opsrc);
            instruction_pointer++;
            continue;
        }
        else if (line.starts_with("put"))
        {
            line = line.substring(sizeof("put"));
            auto identifier = line.substring(line.begin(), line.find(" "));
            if (!frames.last().objects.contains(identifier))
                exit(-1);
            frames.last().backpack.insert_or_assign(identifier, frames.last().objects.at(identifier));
            instruction_pointer++;
            continue;
        }
        else if (line.starts_with("notice the number in"))
        {
            line = line.substring(sizeof("notice the number in"));
            auto identifier = line.substring(line.begin(), line.find(" "));
            if (!frames.last().objects.contains(identifier))
                exit(-1);
            line = line.substring(identifier.length()+1);
            if (line.starts_with("is slightly greater"))
            {
                frames.last().objects.insert_or_assign(identifier, as_num(frames.last().objects.at(identifier))+1);
            }
            else if (line.starts_with("is slightly smaller"))
            {
                frames.last().objects.insert_or_assign(identifier, as_num(frames.last().objects.at(identifier))-1);
            }
            else if (line.starts_with("is exactly greater by"))
            {
                line = line.substring(sizeof("is exactly greater by"));
                auto literal =  line.substring(line.begin(), line.find(" "));
                auto imm = to_num(literal);
                frames.last().objects.insert_or_assign(identifier, as_num(frames.last().objects.at(identifier))+imm);
            }
            else if (line.starts_with("is exactly smaller by"))
            {
                line = line.substring(sizeof("is exactly smaller by"));
                auto literal =  line.substring(line.begin(), line.find(" "));
                auto imm = to_num(literal);
                frames.last().objects.insert_or_assign(identifier, as_num(frames.last().objects.at(identifier))-imm);
            }
            else if (line.starts_with("is the number in"))
            {
                line = line.substring(sizeof("is the number in"));
                auto src1 = line.substring(line.begin(), line.find(" "));
                ssize_t src1val;
                if (!frames.last().objects.contains(src1))
                    src1val = to_num(src1);
                else
                    src1val = as_num(frames.last().objects.at(src1));
                line = line.substring(src1.length()+1);
                String operation;
                if (line.starts_with("multiplied by"))
                    operation = "multiplied by";
                else if (line.starts_with("divided by"))
                    operation = "divided by";
                else if (line.starts_with("plus"))
                    operation = "plus";
                else if (line.starts_with("minus"))
                    operation = "minus";
                else
                    exit(-1);
                line = line.substring(operation.length()+1);
                ssize_t src2val;
                if (line.starts_with("the number in"))
                {
                    line = line.substring(sizeof("the number in"));
                    auto src2 = line.substring(line.begin(), line.find(" "));
                    if (!frames.last().objects.contains(src2))
                        exit(-1);
                    else
                        src2val = as_num(frames.last().objects.at(src2));
                }
                else
                {
                    src2val = to_num(line);
                }
                
                if (operation == "multiplied by"_s)
                {
                    frames.last().objects.insert_or_assign(identifier, src1val * src2val);
                }
                if (operation == "divided by"_s)
                {
                    frames.last().objects.insert_or_assign(identifier, src1val / src2val);
                }
                if (operation == "plus"_s)
                {
                    frames.last().objects.insert_or_assign(identifier, src1val + src2val);
                }
                if (operation == "minus"_s)
                {
                    frames.last().objects.insert_or_assign(identifier, src1val - src2val);
                }
            }
            instruction_pointer++;
            continue;
        }
        else if (line.starts_with("ask myself"))
        {
            line = line.substring(sizeof("ask myself")+1);
            auto question = line.substring(line.begin(), line.find("\""));
            line = line.substring(question.length()+2);
            if (!line.starts_with("and write the answer in"))
                exit(-1);
            line = line.substring(sizeof("and write the answer in"));
            auto identifier = line.substring(line.begin(), line.find(" "));
            if (!frames.last().objects.contains(identifier))
                exit(-1);
            char buf[1025] {0};
            printf("%s ", question.null_terminated_characters());
            [[maybe_unused]] auto _ = scanf("%s", buf);
            String str(buf);
            frames.last().objects.insert_or_assign(identifier, str);
            instruction_pointer++;
            continue;
        }
        else if (line.starts_with("it feels like time doesn't pass"))
        {
            printf("It feels like time doesn't pass...\n");
            instruction_pointer++;
            continue;
        }
        else if (line.starts_with("nap for as many hours as it's written in"))
        {
            line = line.substring(sizeof("nap for as many hours as it's written in"));
            auto identifier = line.substring(line.begin(), line.find(" "));
            if (!frames.last().objects.contains(identifier) || frames.last().objects.at(identifier).has_num)
                exit(-1);
            Time now = Time::from_string(lines[instruction_pointer].substring_view(0, 5));
            Time naptime = Time::from_string(frames.last().objects.at(identifier).str);
            Time new_time_ip = now + naptime;
            instruction_pointer = lines.filter([](StringView const& sv) { return !sv.starts_with("at"); }).find([&](StringView const& sv) { return new_time_ip < Time::from_string(sv.substring_view(0, 5)); }).index();
            continue;
        }
        else if (line.starts_with("Good heavens, just look at the time! It's"))
        {
            line = line.substring(sizeof("Good heavens, just look at the time! It's"));
            auto new_time = line.substring(0, 5);
            instruction_pointer = lines.find([&](StringView const& s) { return s.starts_with(new_time); }).index();
            continue;
        }
        else if (line.starts_with("erase"))
        {
            line = line.substring(sizeof("erase"));
            auto identifier = line.substring(line.begin(), line.find(" "));
            if (!frames.last().objects.contains(identifier))
                exit(-1);
            frames.last().objects.insert_or_assign(identifier, ClockObject{});
            instruction_pointer++;
            continue;
        }
        else
        {
            // print verbatim
            printf("%s\n", String(lines[instruction_pointer++]).null_terminated_characters());
            continue;
        }
        
    }
    
    return 0;
};


int main(int argc, char** argv)
{
    if (argc < 2)
    {
        fprintf(stderr, "Error: Please pass a code file as first argument\n");
        return -1;
    }
    
    auto file_or_error = File::read_all(argv[1]);
    if (file_or_error.has_error())
    {
        fprintf(stderr, "Error reading file: %s\n", strerror((int)file_or_error.error()));
        return -1;
    }
    
    auto buffer = move(file_or_error.result());
    StringView sv((char*)buffer.data(), buffer.size());
    
    interpret(sv);
    return 0;
}
