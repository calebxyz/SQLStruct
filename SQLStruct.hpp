// Type your code here, or load an example.
#include <memory> 
#include <vector> 
#include <type_traits>
#include <map>
#include <string>
#include <iostream>
#include <variant>
#include <cstdint>
#include <cmath>
#include <math.h>
#include <algorithm>
#include <any>
#include <charconv>
#include <utility>
#include <array>
#include <concepts>
#include <string>

using namespace std::literals;

struct empty{};

template <typename Specialization, template <typename...> typename Basetype>
concept specialization_of_types_only = requires (Specialization s) { 
    []()-> decltype(Basetype(s))* {return nullptr;}.operator()();
};


//evil and ugly code , maybe i will find a better solution in the future 
#define create_specialization_concept(Name, Basetype)                          \
template <typename Specialization>                                             \
concept specialization_##Name = requires (Specialization s){                   \
    []()-> decltype(Basetype(s))* {return nullptr;}.operator()();                              \
}

#define create_specialization_type_name(Basetype) create_specialization_concept(Basetype, Basetype)

template <typename T>
concept is_empty = std::same_as<T, empty>;


static constexpr std::size_t get_integral_size(std::integral auto i){
    std::size_t count{0};
    auto num = i;
    while (num > 0) {
        count++;
        num /= 10;
    }

    return count; 
}

template <std::size_t Size>
struct fixed_string {
    char _data[Size + 1]{0};
    static constexpr std::size_t _size = Size;

    constexpr explicit(false) fixed_string(char const* str) {
        std::copy_n(str, Size + 1, _data);
    }

    template <std::integral Integral> 
    constexpr explicit(false) fixed_string(Integral i){
        number_to_string(i, get_integral_size(i));
    }

    constexpr explicit(false) fixed_string() {
    }

    constexpr explicit(false) operator std::string_view() const {
        return {_data, _size};
    }

    constexpr std::size_t size() const {
        return _size;
    }

    constexpr const char* data() const {
        return _data;
    }

    constexpr auto operator<=>(const fixed_string&) const = default;

    private: 
    
//constexpr algorithm ro turn numbers to strings 
// in c++23 we will be able to use to_chars
constexpr void number_to_string(std::integral auto i, std::size_t count){
    auto num = i; 

    while (count > 0){
        count--;
        char c = '0' + num % 10; 
        _data[count] = c; 
        num /= 10;
    }
}
};

create_specialization_type_name(fixed_string);


//helper for empty fixed_string
fixed_string() -> fixed_string<0>;

template <unsigned int Size> 
fixed_string(char const (&)[Size]) -> fixed_string<Size - 1>;

template <auto i> requires std::integral<decltype(i)>
constexpr auto make_integral_fixed_string() {
    return fixed_string<get_integral_size(i)>(i);
}

template <std::size_t N>
struct IDXWrapper{
    static constexpr std::size_t IDX = N;

};


template <typename T>
concept IDXWrapperable = requires (T) { T::IDX; }; 

template<auto Key, typename Val>
struct alignas(alignof(int)) schema_field {
    using ArgType = Val;
    static constexpr auto key_ = Key;
    using Pure_Key_Type = std::decay_t<decltype(Key)>; 
    Val val_{};
     

    template<typename T>
    constexpr auto operator=(const T& val) const {
        return schema_field<Key, T>{.val_ = val};
    }

    constexpr explicit(false) operator std::string_view() const {
        return _strKey;
    }

    private:
    static constexpr auto get_str_key() requires specialization_fixed_string<Pure_Key_Type> and 
    (not std::integral<Pure_Key_Type>){
        return Key;
    }

    static constexpr auto get_str_key() requires std::integral<Pure_Key_Type>{
        return make_integral_fixed_string<Key>();
    }

    static constexpr auto get_str_key() requires std::convertible_to<Pure_Key_Type, std::string_view> and 
    (not std::integral<Pure_Key_Type>) and (not specialization_fixed_string<Pure_Key_Type>){
        return fixed_string<Key.size()>(Key.data());
    }

    static constexpr auto _strKey = get_str_key();
    static constexpr std::size_t _size = _strKey.size();
};


create_specialization_type_name(schema_field);

constexpr std::size_t integral_pow(std::integral auto base, std::integral auto exp){
    auto ret{base};
    for (decltype(base) i = 1; i < static_cast<decltype(base)>(exp); i++){
        ret*=base;
    }

    return exp > 0 ? ret : 1;
}

//again only in c++23 we will be able to use from_chars as a constexpr 
//this funcion will produce an error if a number of size greater then std::numeric_limits<std::size_t>::max() will be used 
template<char... data> 
requires (sizeof...(data) < get_integral_size(std::numeric_limits<std::size_t>::max()))
constexpr std::size_t to_number(){ 
    constexpr auto size_of_pack = sizeof...(data)-1;
    return []<std::size_t... Idxs>(std::index_sequence<Idxs...>){
        return (... + ((data-'0')*integral_pow(10, size_of_pack-Idxs)));
    }(std::make_index_sequence<sizeof...(data)>());
}

template<fixed_string Name>
constexpr auto operator""_sf() { return schema_field<Name, empty>{}; };

template<fixed_string Name>
constexpr auto operator""_fs() { return Name; };

template<char... data> 
constexpr auto operator""_isf(){
    constexpr std::size_t idx = to_number<data...>();
    return schema_field<idx, schema_field<""_fs, empty>>{};
}

template <char... data>
constexpr auto operator""_IDX(){
   return IDXWrapper<to_number<data...>()>();
}


template<specialization_schema_field... schema_fields> 
struct schema : public schema_fields...{}; 

namespace impl{
    //if we cant get the type use this function to return the default type 
    template<typename Default, auto Key, template <auto, typename> typename>
    auto find_sliced_type(...) -> Default;

    //if we can deduce the argument type then this function will return the correct argument type that was inhereted from 
    // this works due to the fact that sqlstruct inherits all arguments 
    template<typename, auto Key, template <auto, typename> typename Arg, typename Val>
    requires (std::integral<decltype(Key)> || std::convertible_to<decltype(Key), std::string_view>)
    auto find_sliced_type(Arg<Key, Val>*) -> Arg<Key, Val>;  
}

template<typename MultiStruct, typename Default, auto Key, template <auto, typename> typename Arg>
using find_sliced_type = decltype(impl::find_sliced_type<Default, Key, Arg>(static_cast<MultiStruct*>(nullptr)));

template <specialization_schema_field... schema_fields> 
struct SQLRow : public schema_fields... {
    template<std::size_t... Ns>
    static constexpr decltype(auto) get_type_for_ind(std::index_sequence<Ns...>){
        return schema<
        schema_field<static_cast<std::size_t>(Ns), schema_fields>...>{};
    }

    constexpr explicit(true) SQLRow(schema_fields... fields): schema_fields{fields}...
    {};

    //default constructor that allocates nothing
    //TODO: maybe we need to do something here so that users wont be able to use it 
    constexpr explicit(true) SQLRow() {};

    //this is just amazing we are passing non template parameters and the deduction rules 
    //just know how to complete them!!!!
    template <typename T, typename ARG = find_sliced_type<SQLRow, empty, T::key_, schema_field>>
    requires (!std::same_as<empty, ARG>)
    constexpr const auto& operator[](const T) const {
        return static_cast<const ARG*>(this)->val_;
    }

    template <typename T, typename ARG = find_sliced_type<SQLRow, empty, T::key_, schema_field>>
    requires (!std::same_as<empty, ARG>)
    constexpr auto& operator[](const T) {
        return static_cast<ARG*>(this)->val_;
    }

    template <IDXWrapperable T>
    constexpr const auto& operator[](const T) const {
        return get<T::IDX>().val_;
    }

    template <IDXWrapperable T>
    constexpr auto& operator[](const T) {
        return get<T::IDX>().val_;
    }

    template <std::size_t N, 
    typename ArgMap = std::decay_t<decltype(get_type_for_ind(std::make_index_sequence<sizeof...(schema_fields)>{}))>,
    typename Arg = find_sliced_type<ArgMap, empty, N, schema_field>>
    auto& get() requires (!std::same_as<empty, Arg>) {
        return *static_cast<typename Arg::ArgType*>(this);
    }

    template <std::size_t N, 
    typename ArgMap = std::decay_t<decltype(get_type_for_ind(std::make_index_sequence<sizeof...(schema_fields)>{}))>,
    typename Arg = find_sliced_type<ArgMap, empty, N, schema_field>>
    const auto& get() const requires (!std::same_as<empty, Arg>) {
        return *static_cast<const typename Arg::ArgType*>(this);
    }

    static constexpr std::size_t size = sizeof...(schema_fields);
};

template <specialization_schema_field... schema_fields>
SQLRow(schema_fields...) -> SQLRow<schema_fields...>; 

create_specialization_type_name(SQLRow);

namespace std{

    template<specialization_schema_field... schema_fields>
    struct tuple_size<SQLRow<schema_fields...>>{
        static constexpr size_t value = SQLRow<schema_fields...>::size;
    };

    template <size_t IDX, specialization_schema_field... schema_fields>
    struct tuple_element<IDX, SQLRow<schema_fields...>>{
        using SQL = SQLRow<schema_fields...>;
        using ArgMap = decltype(SQL::get_type_for_ind(std::make_index_sequence<sizeof...(schema_fields)>{}));
        using type = typename find_sliced_type<ArgMap, ::empty, IDX, schema_field>::ArgType;
        static_assert(!is_same_v<type, ::empty>);
    };

    template <size_t IDX, specialization_SQLRow Row>
    auto& get(Row& sql){
        return sql.template get<IDX>();
    } 

    template <size_t IDX, specialization_SQLRow Row>
    const auto& get(const Row& sql){
        return sql.template get<IDX>();
    } 
}
 

//for now we will hold the rows as an array to get constexpr p
template <std::size_t N, specialization_SQLRow Row>
class SQLTable{
    public: 
    constexpr SQLTable(specialization_SQLRow auto&&... rows) 
    requires std::conjunction_v<std::is_same<Row, std::decay_t<decltype(rows)>>...>{
        rows_ = std::to_array({rows...});
    }

    auto operator[](std::size_t row_idx){
        return rows_[row_idx];
    }

    private: 
    std::array<Row, N> rows_;
};

template <specialization_SQLRow Head, specialization_SQLRow... Tail>
SQLTable(Head, Tail...) -> SQLTable<sizeof...(Tail)+1, Head>;

int main(){
    constexpr auto key = (158_isf).key_;
    std::cout << key << "\n";
    constexpr auto fs = "Itai"_fs;
    static_assert(fs == "Itai"sv);
    static_assert("Ari"sv == "Ari"_sf);
    auto arg1 = ("Ari"_sf = 10);
    constexpr auto sql1 = SQLRow("x"_sf = 10, "y"_sf = 20.05f);
    static_assert(10 == sql1["x"_sf]);
    auto sql2 = SQLRow("x"_sf = 10, "y"_sf = 20.05f, "Itay"_sf = std::array<int, 3>{{1, 2, 3}});
    sql2["x"_sf] = 11;
    sql2["y"_sf] = 20.11f;
    sql2["Itay"_sf][2] = 5; 
    auto arg3 = std::get<0>(sql2);
    auto& [a1, a2, a3] = sql2;
    auto table = SQLTable(SQLRow("x"_sf = 10, "y"_sf = 20.05f), SQLRow("x"_sf = 1, "y"_sf = 20.15f));
    auto test3 = std::get<0>(table[0]);
    auto test4 = table[0][1_IDX];
    return arg3.val_ + a1.val_;
}
