module.exports = (fun, num) ->
    return if num <= 0
    fun()
    this fun, --num
